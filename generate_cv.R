# generate_cv.R
# Generates a single LaTeX file for the CV from cv.yaml

library(yaml)
library(stringr)

# --- CONFIG ---
template_file <- "resume.tex"
output_file <- "cv_complete.tex"
yaml_file <- "cv.yaml"

# --- HELPER FUNCTIONS ---

clean_latex_field <- function(text) {
    if (is.null(text) || is.na(text)) {
        return("")
    }
    text <- as.character(text)

    # 1. Extract markdown links: [text](url) -> \href{url}{text}
    link_pattern <- "\\[(.*?)\\]\\((.*?)\\)"

    locs <- str_locate_all(text, link_pattern)[[1]]
    links <- list()
    if (nrow(locs) > 0) {
        for (i in nrow(locs):1) {
            full_match <- substr(text, locs[i, 1], locs[i, 2])
            groups <- str_match(full_match, link_pattern)
            link_text <- groups[1, 2]
            link_url <- groups[1, 3]

            # Safe placeholder
            placeholder <- paste0("ZZLINKZZ", i)
            links[[placeholder]] <- list(text = link_text, url = link_url)

            text <- paste0(
                substr(text, 1, locs[i, 1] - 1),
                placeholder,
                substr(text, locs[i, 2] + 1, nchar(text))
            )
        }
    }

    # 2. Escape special chars in TEXT (but not in URL placeholders)
    chars_to_escape <- c("&", "%", "\\$", "#", "_")
    for (ch in chars_to_escape) {
        text <- str_replace_all(text, ch, paste0("\\\\", ch))
    }

    # 3. Restore links
    for (ph in names(links)) {
        orig <- links[[ph]]
        # Escape latex in display text of the link
        safe_text <- orig$text
        for (ch in chars_to_escape) {
            safe_text <- str_replace_all(safe_text, ch, paste0("\\\\", ch))
        }

        latex_link <- sprintf("\\href{%s}{%s}", orig$url, safe_text)
        # Use gsub with fixed=TRUE
        text <- gsub(ph, latex_link, text, fixed = TRUE)
    }

    return(text)
}

# --- GENERATE SECTIONS ---

data <- yaml::read_yaml(yaml_file)
`%||%` <- function(a, b) if (is.null(a)) b else a

# 1. HEADER
gen_header <- function() {
    l <- ""
    l <- paste0(
        l,
        "\\newlength{\\rcollength}\\setlength{\\rcollength}{1.85in}%\n"
    )
    l <- paste0(
        l,
        "\\newlength{\\spacewidth}\\setlength{\\spacewidth}{20pt}%\n"
    )
    l <- paste0(l, "%\n")
    l <- paste0(
        l,
        "\\begin{tabular}[t]{@{}p{\\textwidth-\\rcollength-\\spacewidth}@{}p{\\spacewidth}@{}p{\\rcollength}}%\n"
    )

    # Address
    univ <- "University of Maryland"
    loc <- data$location %||% "College Park, MD, 20742"

    l <- paste0(l, "\\parbox{\\textwidth-\\rcollength-\\spacewidth}{%\n")
    l <- paste0(
        l,
        sprintf(
            "  {%s}\\\\\n  {%s} \\\\\n  %s}\n\n",
            "1401 Marie Mount Hall",
            univ,
            loc
        )
    )

    # SPACING FIX: Skip the spacer column!
    l <- paste0(l, "& \n") # Move to spacer
    l <- paste0(l, "& \n") # Move to right col

    # Contact - WIDTH FIX: Use \rcollength
    l <- paste0(l, "\\parbox{\\rcollength}{%\n")
    if (!is.null(data$website)) {
        l <- paste0(
            l,
            sprintf(
                "\\textit{Website:} \\hspace{0.5em} \\href{https://%s}{%s}\\\\\n",
                data$website,
                data$website
            )
        )
    }

    github <- data$github
    twitter <- data$twitter
    socials <- c()
    if (!is.null(github)) {
        socials <- c(
            socials,
            sprintf("\\href{http://github.com/%s}{Github}", github)
        )
    }
    if (!is.null(twitter)) {
        socials <- c(
            socials,
            sprintf("\\href{https://twitter.com/%s}{Twitter}", twitter)
        )
    }
    if (length(socials) > 0) {
        l <- paste0(
            l,
            sprintf(
                "\\textit{Social:} \\hspace{1em} %s\\\\\n",
                paste(socials, collapse = " | ")
            )
        )
    }

    if (!is.null(data$office)) {
        l <- paste0(
            l,
            sprintf("\\textit{Office:} \\hspace{1em} %s \\\\\n", data$office)
        )
    }

    l <- paste0(
        l,
        sprintf("\\textit{E-mail:} \\hspace{1em}\\email{%s}}\n", data$email)
    )
    l <- paste0(l, "\n\\end{tabular}\n")
    return(l)
}

# 2. EDUCATION
gen_education <- function() {
    l <- "\\section{education}\n\n"
    for (edu in data$education) {
        inst <- clean_latex_field(edu$institution)
        loc <- clean_latex_field(edu$location %||% "")
        year <- clean_latex_field(edu$year %||% "")
        degree <- clean_latex_field(edu$degree %||% "")

        # Line 1: Inst, Loc, Year. Always ends with \\
        l <- paste0(
            l,
            sprintf("\\textbf{%s}, %s \\hfill %s\\\\\n", inst, loc, year)
        )

        # Line 2: Degree.
        l <- paste0(l, sprintf("%s", degree))

        if (!is.null(edu$details)) {
            l <- paste0(l, "\\\\\n") # Break after degree
            d_len <- length(edu$details)
            for (i in 1:d_len) {
                l <- paste0(l, clean_latex_field(edu$details[[i]]))
                if (i < d_len) {
                    l <- paste0(l, "\\\\\n")
                } else {
                    l <- paste0(l, "\n")
                } # Last detail has NO backslashes
            }
        } else {
            l <- paste0(l, "\n") # No details, last line has NO backslashes
        }

        l <- paste0(l, "\n\\halfblankline\n\n")
    }
    return(l)
}

# 3. RESEARCH PROJECTS
gen_projects <- function() {
    l <- "\\section{involvement in research projects}\n"
    l <- paste0(l, "\\restartlist{bibenum}\n\\setlength{\\bibhang}{0pt}\n\n")
    l <- paste0(
        l,
        "\\begin{bibsection}\n  \\item \\textbf{As a researcher}\n\\end{bibsection}\n"
    )
    l <- paste0(l, "\\blankline\n\\begin{bibenum}\n")

    for (proj in data$projects) {
        ti <- clean_latex_field(proj$title)
        url <- proj$url
        fund <- clean_latex_field(proj$funding %||% "")
        per <- clean_latex_field(proj$period %||% "")
        pi <- clean_latex_field(proj$pi %||% "")
        desc <- clean_latex_field(proj$description %||% "")

        if (!is.null(url)) {
            title_str <- sprintf("\\href{%s}{\\emph{%s}}", url, ti)
        } else {
            title_str <- sprintf("\\emph{%s}", ti)
        }

        l <- paste0(
            l,
            sprintf(
                "  \\item Project title: %s (Funded by %s, %s).\\\\\n",
                title_str,
                fund,
                per
            )
        )
        if (pi != "") {
            l <- paste0(l, sprintf("  Principal Investigator: %s", pi))
            if (!is.null(proj$partners)) {
                l <- paste0(
                    l,
                    sprintf(
                        "; Project partners: %s",
                        clean_latex_field(proj$partners)
                    )
                )
            }
            if (!is.null(proj$cooperator)) {
                l <- paste0(
                    l,
                    sprintf(
                        ", Cooperator: %s",
                        clean_latex_field(proj$cooperator)
                    )
                )
            }
            l <- paste0(l, ".\n")
        }
        l <- paste0(l, "  \\begin{innerlist}\n")
        l <- paste0(l, sprintf("    \\item[] \\emph{%s}\n", desc))
        l <- paste0(l, "  \\end{innerlist}\n\n")
    }
    l <- paste0(l, "\\end{bibenum}\n\\blankline\n")
    return(l)
}

# 4. TEACHING
gen_teaching <- function() {
    l <- "\\section{teaching experience}\n"
    for (exp in data$teaching) {
        inst <- clean_latex_field(exp$institution)
        loc <- clean_latex_field(exp$location)

        l <- paste0(l, sprintf("\\textbf{%s}, %s\n", inst, loc))
        # FIX: Add blank line before outerlist and blank lines around halfblankline
        l <- paste0(
            l,
            "\n\\halfblankline\n\n\\begin{outerlist}[nolistsep,noitemsep]\n"
        )

        if (!is.null(exp$items)) {
            for (item in exp$items) {
                role <- item$role
                dept <- item$department
                year <- item$year
                l <- paste0(
                    l,
                    sprintf(
                        "    \\item \\textit{%s} at the %s \\hfill %s \\vspace{0.3em}\n",
                        role,
                        dept,
                        year
                    )
                )
                l <- paste0(l, "    \\begin{innerlist}\n")

                if (!is.null(item$courses)) {
                    for (c in item$courses) {
                        line <- sprintf("\\teachingterm{%s}", c$term)
                        if (!is.null(c$code)) {
                            line <- paste0(
                                line,
                                sprintf(
                                    " \\hspace{0.78em} %s: %s",
                                    c$code,
                                    clean_latex_field(c$name)
                                )
                            )
                        } else {
                            line <- paste0(
                                line,
                                sprintf(": %s", clean_latex_field(c$name))
                            )
                        }

                        if (!is.null(c$syllabus)) {
                            line <- paste0(
                                line,
                                sprintf(
                                    " [\\href{%s}{\\texttt{\\textbf{syl}}}]",
                                    c$syllabus
                                )
                            )
                        }
                        l <- paste0(
                            l,
                            sprintf("        \\item[] \\hspace{-1em}%s\n", line)
                        )
                    }
                }
                l <- paste0(l, "    \\end{innerlist}\n")
            }
        }
        # Fix: Blank lines around halfblankline at end
        l <- paste0(
            l,
            "\\end{outerlist}\n\n\\halfblankline\n\n\\halfblankline\n\n"
        )
    }
    return(l)
}

# 5. PUBLICATIONS
gen_pubs <- function() {
    pubs <- data$publications

    gen_list <- function(items, title, restart = TRUE) {
        if (is.null(items) || length(items) == 0) {
            return("")
        }

        l <- sprintf("\\section{%s}\n", title)
        if (restart) {
            l <- paste0(l, "\\restartlist{bibenum}\n")
        }
        l <- paste0(l, "\\begin{bibenum}\n")

        for (item in items) {
            year <- clean_latex_field(item$year %||% "")
            ti <- clean_latex_field(item$title %||% "")
            container <- clean_latex_field(item$container %||% "")
            auth <- clean_latex_field(item$author %||% "")

            my_names <- c(
                "Türk, Utku",
                "T\"urk, Utku",
                "Utku Türk",
                "Utku T\"urk",
                "T{\\\"u}rk, Utku"
            )
            auth_l <- auth
            for (n in my_names) {
                auth_l <- gsub(
                    n,
                    sprintf("\\textbf{%s}", n),
                    auth_l,
                    fixed = TRUE
                )
            }

            # Generate icon links from the 'links' list
            icon_links <- ""
            if (!is.null(item$links)) {
                link_strs <- c()
                for (link in item$links) {
                    icon <- ""
                    if (link$name == "doi") {
                        icon <- "\\faBarcode"
                    } else if (link$name == "paper") {
                        icon <- "\\faFilePdf"
                    } else if (link$name == "abstract") {
                        icon <- "\\faStickyNote"
                    } else if (link$name == "slides") {
                        icon <- "\\faTv"
                    } else if (link$name == "poster") {
                        icon <- "\\faImage"
                    } else if (link$name == "handout") {
                        icon <- "\\faFile"
                    } else if (link$name == "osf") {
                        icon <- "\\faDatabase"
                    } else if (link$name == "github" || link$name == "repo") {
                        icon <- "\\faGithub"
                    } else if (link$name == "biorxiv") {
                        icon <- "\\faDna"
                    } else if (link$name == "arxiv") {
                        icon <- "\\faArchive"
                    } else {
                        icon <- "\\faLink"
                    }

                    link_strs <- c(
                        link_strs,
                        sprintf("\\href{%s}{%s}", link$url, icon)
                    )
                }
                icon_links <- paste(link_strs, collapse = " ")
            }

            # Add DOI link if not already in links (legacy support) but priority to links list
            if (!is.null(item$doi) && is.null(item$links)) {
                raw_doi <- item$doi
                clean_doi <- gsub("https://doi.org/", "", raw_doi)
                icon_links <- sprintf("\\href{%s}{\\faBarcode}", raw_doi)
            } else if (!is.null(item$url) && is.null(item$links)) {
                icon_links <- sprintf("\\href{%s}{\\faLink}", item$url)
            }

            # Determine punctuation after title
            title_punc <- "."
            if (grepl("[?!]$", ti)) {
                title_punc <- ""
            }

            # Prepare container string
            container_str <- ""
            if (container != "") {
                container_str <- sprintf(" \\emph{%s}.", container)
            }

            # Construct the line
            if (year != "") {
                line_l <- sprintf(
                    "\\item %s. %s. %s%s%s %s",
                    auth_l,
                    year,
                    ti,
                    title_punc,
                    container_str,
                    icon_links
                )
            } else {
                line_l <- sprintf(
                    "\\item %s. %s%s%s %s",
                    auth_l,
                    ti,
                    title_punc,
                    container_str,
                    icon_links
                )
            }
            l <- paste0(l, line_l, "\n")
        }
        l <- paste0(l, "\\end{bibenum}\n\\halfblankline\n\n")
        return(l)
    }

    out <- list()

    p_str <- gen_list(pubs$articles, "refereed articles")
    p_str <- paste0(p_str, gen_list(pubs$chapters, "chapters"))
    p_str <- paste0(
        p_str,
        gen_list(pubs$proceedings, "conference publications")
    )
    p_str <- paste0(
        p_str,
        gen_list(pubs$in_prep, "in preparation / submitted")
    )
    out$pubs <- p_str

    t_str <- gen_list(pubs$talks, "talks")
    t_str <- paste0(t_str, gen_list(pubs$posters, "posters (refereed)"))
    t_str <- paste0(t_str, gen_list(pubs$tutorials, "tutorials"))
    out$talks <- t_str

    return(out)
}


# --- EMBEDDED TEMPLATES ---

latex_settings <- "
\\usepackage{times}
\\usepackage{calc}
\\usepackage[shortcuts]{extdash}
\\usepackage{fontawesome5}
\\reversemarginpar
\\usepackage[paper=a4paper,
            marginparwidth=25.5mm,   
            marginparsep=5mm,       
            margin=20mm,              
            includemp]{geometry}
\\setlength{\\parindent}{0in}
\\usepackage[shortlabels]{enumitem}
\\makeatletter
\\newlength{\\bibhang}
\\setlength{\\bibhang}{1em}
\\newlength{\\bibsep}
 {\\@listi \\global\\bibsep\\itemsep \\global\\advance\\bibsep by\\parsep}
\\newlist{bibsection}{itemize}{3}
\\setlist[bibsection]{label=,leftmargin=\\bibhang,%
        itemindent=-\\bibhang,
        itemsep=\\bibsep,parsep=\\z@,partopsep=0pt,
        topsep=0pt}
\\newlist{bibenum}{enumerate}{3}
\\setlist[bibenum]{label=[\\arabic*],resume,leftmargin={\\bibhang+\\widthof{[999]}},%
        itemindent=-\\bibhang,
        itemsep=3pt,parsep=1pt,partopsep=0pt,
        topsep=0pt}
\\let\\oldendbibenum\\endbibenum
\\def\\endbibenum{\\oldendbibenum\\vspace{0.5\\baselineskip}}
\\let\\oldendbibsection\\endbibsection
\\def\\endbibsection{\\oldendbibsection\\vspace{0.5\\baselineskip}}
\\makeatother
\\usepackage{fancyhdr,lastpage}
\\pagestyle{fancy}
\\fancyhf{}\\renewcommand{\\headrulewidth}{0pt}
\\fancyfootoffset{\\marginparsep+\\marginparwidth}
\\newlength{\\footpageshift}
\\setlength{\\footpageshift}
          {0.5\\textwidth+0.5\\marginparsep+0.5\\marginparwidth-2in}
\\lfoot{\\hspace{\\footpageshift}%
       \\parbox{4in}{\\, \\hfill %
                    \\arabic{page} of \\protect\\pageref*{LastPage} % +LP
%                    \\arabic{page}                               % -LP
                    \\hfill \\,}}
\\usepackage{color,hyperref}
\\definecolor{lightgrey}{rgb}{0.6, 0.6, 0.6}
\\definecolor{navyblue}{rgb}{0.0, 0.0, 0.5}
%\\definecolor{navyblue}{rgb}{0.0, 0.0, 0}
\\hypersetup{colorlinks,breaklinks,
            linkcolor=navyblue,urlcolor=navyblue,
            anchorcolor=navyblue,citecolor=navyblue}
\\newcommand{\\makeheading}[2][]%
        {\\hspace*{-\\marginparsep minus \\marginparwidth}%
         \\begin{minipage}[t]{\\textwidth+\\marginparwidth+\\marginparsep}%
             {\\large \\bfseries #2 \\hfill #1}\\\\[-0.15\\baselineskip]%
                 \\rule{\\columnwidth}{1pt}%
         \\end{minipage}}
\\renewcommand{\\section}[1]{\\pagebreak[3]%
    \\vspace{1.5\\baselineskip}%
    \\phantomsection\\addcontentsline{toc}{section}{#1}%
    \\noindent\\llap{\\scshape\\smash{\\parbox[t]{\\marginparwidth}{\\hyphenpenalty=10000\\raggedleft #1}}\\hspace{\\marginparsep}}%
    \\vspace{-\\baselineskip}\\par}
\\newcommand{\\teachingterm}[1]{\\makebox[6em][l]{\\textsc{#1}}}
\\newcommand*\\fixendlist[1]{%
    \\expandafter\\let\\csname preFixEndListend#1\\expandafter\\endcsname\\csname end#1\\endcsname
    \\expandafter\\def\\csname end#1\\endcsname{\\csname preFixEndListend#1\\endcsname\\vspace{0.5\\baselineskip}}}
\\let\\originalItem\\item
\\newcommand*\\fixouterlist[1]{%
    \\expandafter\\let\\csname preFixOuterList#1\\expandafter\\endcsname\\csname #1\\endcsname
    \\expandafter\\def\\csname #1\\endcsname{\\let\\oldItem\\item\\def\\item{\\pagebreak[2]\\oldItem}\\csname preFixOuterList#1\\endcsname}
    \\expandafter\\let\\csname preFixOuterListend#1\\expandafter\\endcsname\\csname end#1\\endcsname
    \\expandafter\\def\\csname end#1\\endcsname{\\let\\item\\oldItem\\csname preFixOuterListend#1\\endcsname}}
\\newcommand*\\fixinnerlist[1]{%
    \\expandafter\\let\\csname preFixInnerList#1\\expandafter\\endcsname\\csname #1\\endcsname
    \\expandafter\\def\\csname #1\\endcsname{\\let\\oldItem\\item\\let\\item\\originalItem\\csname preFixInnerList#1\\endcsname}
    \\expandafter\\let\\csname preFixInnerListend#1\\expandafter\\endcsname\\csname end#1\\endcsname
    \\expandafter\\def\\csname end#1\\endcsname{\\csname preFixInnerListend#1\\endcsname\\let\\item\\oldItem}}
\\newlist{outerlist}{itemize}{3}
    \\setlist[outerlist]{label=\\enskip\\textbullet,leftmargin=*}
    \\fixendlist{outerlist}
    \\fixouterlist{outerlist}
\\newlist{lonelist}{itemize}{3}
    \\setlist[lonelist]{label=\\enskip\\textbullet,leftmargin=*,partopsep=0pt,topsep=0pt}
    \\fixendlist{lonelist}
    \\fixouterlist{lonelist}
\\newlist{innerlist}{itemize}{3}
    \\setlist[innerlist]{label=\\enskip\\textbullet,leftmargin=*,parsep=0pt,itemsep=3pt,topsep=0pt,partopsep=0pt}
    \\fixinnerlist{innerlist}
\\newlist{loneinnerlist}{itemize}{3}
    \\setlist[loneinnerlist]{label=\\enskip\\textbullet,leftmargin=*,parsep=0pt,itemsep=3pt,topsep=0pt,partopsep=0pt}
    \\fixendlist{loneinnerlist}
    \\fixinnerlist{loneinnerlist}
\\newcommand{\\blankline}{\\par\\vspace{\\baselineskip}\\pagebreak[3]}
\\newcommand{\\halfblankline}{\\par\\vspace{0.6\\baselineskip}\\pagebreak[3]}
\\usepackage{doi}
\\usepackage[polutonikogreek,english]{babel}
\\usepackage{url}
\\urlstyle{same}
\\providecommand*\\emaillink[1]{\\nolinkurl{#1}}
\\providecommand*\\email[1]{\\href{mailto:#1}{\\emaillink{#1}}}
\\providecommand\\BibTeX{{B\\kern-.05em{\\sc i\\kern-.025em b}\\kern-.08em \\TeX}}
\\providecommand\\Matlab{\\textsc{Matlab}}
\\hyphenation{bio-mim-ic-ry bio-in-spi-ra-tion re-us-a-ble pro-vid-er Media-Wiki I-o-an-na Tou-louse}
"

latex_template <- "
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Simple LaTeX CV Template %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\documentclass[10pt]{article}
\\RequirePackage[T1]{fontenc}

\\input{settings}

%%%%%%%%%%%%%%%%%%%%%%%%% Begin CV  %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\begin{document}
\\makeheading{Utku Turk \\hfill \\small C.V. (\\today{}) \\normalsize}

\\vspace{1em}

\\input{contact}

\\halfblankline

\\input{interests}

\\input{education}

\\input{pubs}

\\input{projects}

\\input{teaching}
 
\\blankline

\\input{talks}

\\blankline

\\input{skills}

\\input{misc}

\\end{document}
"

# --- GENERATORS FOR NEW SECTIONS ---

gen_interests <- function() {
    if (is.null(data$interests)) {
        return("")
    }
    paste0(
        "\\section{interests}\n\n\\textit{",
        clean_latex_field(data$interests),
        "}"
    )
}

gen_skills <- function() {
    if (is.null(data$skills)) {
        return("")
    }

    l <- "\\section{skills}\n\n"
    l <- paste0(
        l,
        "\\begin{tabular}[t]{@{}p{\\textwidth-\\rcollength-\\spacewidth}@{}p{\\spacewidth}@{}p{\\rcollength}}%\n\n"
    )

    # Left column: Languages
    l <- paste0(l, "\\parbox{\\textwidth-\\rcollength-\\spacewidth}{%\n")
    l <- paste0(l, "\\begin{outerlist}[noitemsep,nolistsep]\n")
    l <- paste0(l, "\\item Modern languages\n")
    l <- paste0(l, "\\begin{innerlist}\n")
    for (lang in data$skills$languages) {
        l <- paste0(l, "\\item[$\\diamond$] ", clean_latex_field(lang), "\n")
    }
    l <- paste0(l, "\\end{innerlist}\n")
    l <- paste0(l, "\\end{outerlist}}\n\n")

    l <- paste0(l, "&\n")

    # Right column: Tools
    l <- paste0(l, "\\parbox{\\textwidth-\\rcollength-\\spacewidth}{%\n")
    l <- paste0(l, "\\begin{outerlist}[noitemsep,nolistsep]\n")
    l <- paste0(l, "\\item Programming \\& Experimenting\n")
    l <- paste0(l, "\\begin{innerlist}\n")
    for (tool in data$skills$tools) {
        l <- paste0(l, "\\item[$\\diamond$] ", clean_latex_field(tool), "\n")
    }
    l <- paste0(l, "\\end{innerlist}\n")
    l <- paste0(l, "\\end{outerlist}}\n")

    l <- paste0(l, "\\halfblankline\n\n\\end{tabular}")
    return(l)
}

gen_misc <- function() {
    l <- "\\blankline\n\n\\section{grants and scholarships}\n\n"
    l <- paste0(l, "\\begin{outerlist}[nolistsep]\n")
    if (!is.null(data$grants)) {
        for (grant in data$grants) {
            l <- paste0(l, "    \\item ", clean_latex_field(grant), "\n")
        }
    }
    l <- paste0(l, "\\end{outerlist}\n\n\\blankline\n\n")

    l <- paste0(l, "\\section{service}\n\n\\begin{outerlist}[nolistsep]\n")
    if (!is.null(data$service)) {
        for (svc in data$service) {
            l <- paste0(l, "\\item ", clean_latex_field(svc$group), "\n")
            l <- paste0(l, "\\begin{innerlist}\n")
            for (item in svc$items) {
                l <- paste0(l, "    \\item ", clean_latex_field(item), "\n")
            }
            l <- paste0(l, "\\end{innerlist}\n\\halfblankline\n")
        }
    }
    l <- paste0(l, "\\end{outerlist}\n")

    return(l)
}


# --- MAIN EXECUTION ---

content <- latex_template

# Replace inputs
content <- gsub(
    "\\\\input\\{settings\\}",
    gsub("\\\\", "\\\\\\\\", latex_settings),
    content
)
content <- gsub(
    "\\\\input\\{contact\\}",
    gsub("\\\\", "\\\\\\\\", gen_header()),
    content
)
content <- gsub(
    "\\\\input\\{education\\}",
    gsub("\\\\", "\\\\\\\\", gen_education()),
    content
)

pubs_data <- gen_pubs()
content <- gsub(
    "\\\\input\\{pubs\\}",
    gsub("\\\\", "\\\\\\\\", pubs_data$pubs),
    content
)
content <- gsub(
    "\\\\input\\{talks\\}",
    gsub("\\\\", "\\\\\\\\", pubs_data$talks),
    content
)
content <- gsub(
    "\\\\input\\{projects\\}",
    gsub("\\\\", "\\\\\\\\", gen_projects()),
    content
)
content <- gsub(
    "\\\\input\\{teaching\\}",
    gsub("\\\\", "\\\\\\\\", gen_teaching()),
    content
)
content <- gsub(
    "\\\\input\\{interests\\}",
    gsub("\\\\", "\\\\\\\\", gen_interests()),
    content
)
content <- gsub(
    "\\\\input\\{skills\\}",
    gsub("\\\\", "\\\\\\\\", gen_skills()),
    content
)
content <- gsub(
    "\\\\input\\{misc\\}",
    gsub("\\\\", "\\\\\\\\", gen_misc()),
    content
)


# --- MARKDOWN GENERATORS ---

clean_md_field <- function(text) {
    if (is.null(text) || is.na(text)) {
        return("")
    }
    text <- as.character(text)

    # 0. Formatting cleanup (remove size, simple font adjustments)
    text <- gsub("\\\\scriptsize", "", text)
    text <- gsub("\\\\huge", "", text)
    text <- gsub("\\\\large", "", text)
    text <- gsub("\\\\-", "", text) # Remove soft hyphens

    # 1. Inner tags first (styles)
    # \textsc{text} -> text
    text <- gsub("\\\\textsc\\{([^\\}]*)\\}", "\\1", text)

    # \texttt{text} -> `text`
    text <- gsub("\\\\texttt\\{([^\\}]*)\\}", "`\\1`", text)

    # \textbf{text} -> **text**
    text <- gsub("\\\\textbf\\{([^\\}]*)\\}", "**\\1**", text)

    # \textit{text}, \emph{text} -> *text*
    text <- gsub("\\\\textit\\{([^\\}]*)\\}", "*\\1*", text)
    text <- gsub("\\\\emph\\{([^\\}]*)\\}", "*\\1*", text)

    # 2. Links (Outer)
    # \href{url}{text} -> [text](url)
    text <- gsub("\\\\href\\{([^\\}]*)\\}\\{([^\\}]*)\\}", "[\\2](\\1)", text)

    # \LaTeX{} -> LaTeX
    text <- gsub("\\\\LaTeX\\{\\}", "LaTeX", text)
    text <- gsub("\\\\LaTeX", "LaTeX", text)

    # 3. Final Escape Cleanup
    # Unescape braces \{ \} -> { }
    text <- gsub("\\\\\\{", "{", text)
    text <- gsub("\\\\\\}", "}", text)

    # Escaped chars
    text <- gsub("\\\\&", "&", text)
    text <- gsub("\\\\%", "%", text)
    text <- gsub("\\\\#", "#", text)
    text <- gsub("\\\\_", "_", text)

    # Line breaks
    text <- gsub("\\\\\\\\", " ", text)

    return(text)
}

gen_md_list <- function(items) {
    l <- ""
    for (item in items) {
        l <- paste0(l, "- ", clean_md_field(item), "\n")
    }
    return(l)
}

gen_md_pubs <- function(pub_list) {
    l <- ""
    for (item in pub_list) {
        # Format title
        title <- clean_md_field(item$title)
        # Remove LaTeX braces for Markdown
        title <- gsub("\\{", "", title)
        title <- gsub("\\}", "", title)

        # Format author
        author <- clean_md_field(item$author)
        # Handle special chars if clean_md_field didn't catch them or they are specific to bib
        author <- gsub("\\{\\\\\"u\\}", "ü", author)
        author <- gsub("\\{\\\\\"o\\}", "ö", author)
        author <- gsub("\\{\\\\\"i\\}", "ı", author)
        author <- gsub("\\{\\\\\"s\\}", "ş", author)
        author <- gsub("\\{\\\\\"c\\}", "ç", author)
        author <- gsub("\\{\\\\\"g\\}", "ğ", author)
        author <- gsub("\\\\v\\{c\\}", "ç", author)
        author <- gsub("\\\\v\\{s\\}", "ş", author)
        author <- gsub("\\\\u\\{g\\}", "ğ", author)
        author <- gsub("\\\\c\\{c\\}", "ç", author)
        author <- gsub("\\\\c\\{s\\}", "ş", author)
        author <- gsub("\\{", "", author)
        author <- gsub("\\}", "", author)

        l <- paste0(l, "- **", title, "**. ", author, ". ")
        if (!is.null(item$year)) {
            l <- paste0(l, item$year, ". ")
        }
        if (!is.null(item$container)) {
            container <- clean_md_field(item$container)
            container <- gsub("\\{", "", container)
            container <- gsub("\\}", "", container)
            l <- paste0(l, "*", container, "*. ")
        }

        # Links
        links <- c()
        if (!is.null(item$doi)) {
            links <- c(links, paste0("[DOI](http://doi.org/", item$doi, ")"))
        }
        if (!is.null(item$url)) {
            links <- c(links, paste0("[URL](", item$url, ")"))
        }
        if (!is.null(item$links)) {
            for (link in item$links) {
                links <- c(links, paste0("[", link$name, "](", link$url, ")"))
            }
        }
        if (length(links) > 0) {
            l <- paste0(l, " ", paste(links, collapse = " | "))
        }
        l <- paste0(l, "\n")
    }
    return(l)
}

gen_markdown_cv <- function() {
    pubs <- data$publications
    md <- "---\nlayout: page\ntitle: CV\npermalink: /\n---\n\n"

    # Header
    md <- paste0(md, "# Utku Türk\n\n")
    md <- paste0(
        md,
        "University of Maryland, 1401 Marie Mount Hall, College Park, MD, 20742\n\n"
    )
    md <- paste0(
        md,
        "**Email**: [utkuturk@umd.edu](mailto:utkuturk@umd.edu) | "
    )
    md <- paste0(md, "**Web**: [www.utkuturk.com](https://www.utkuturk.com) | ")
    md <- paste0(md, "**Github**: [utkuturk](http://github.com/utkuturk)\n\n")

    # Interests
    if (!is.null(data$interests)) {
        md <- paste0(
            md,
            "## Interests\n\n",
            clean_md_field(data$interests),
            "\n\n"
        )
    }

    # Education
    md <- paste0(md, "## Education\n\n")
    for (edu in data$education) {
        md <- paste0(md, "### ", edu$school, "\n")
        md <- paste0(md, "*", edu$degree, "*")
        if (!is.null(edu$year)) {
            md <- paste0(md, ", ", edu$year)
        }
        md <- paste0(md, "\n\n")
    }

    # Publications
    md <- paste0(md, "## Publications\n\n")
    md <- paste0(
        md,
        "### Refereed Articles\n\n",
        gen_md_pubs(pubs$articles),
        "\n"
    )
    md <- paste0(md, "### Chapters\n\n", gen_md_pubs(pubs$chapters), "\n")
    md <- paste0(
        md,
        "### Conference Publications\n\n",
        gen_md_pubs(pubs$proceedings),
        "\n"
    )
    md <- paste0(
        md,
        "### In Preparation / Submitted\n\n",
        gen_md_pubs(pubs$in_prep),
        "\n"
    )

    # Projects
    md <- paste0(md, "## Projects\n\n")
    for (proj in data$projects) {
        md <- paste0(md, "### ", proj$title, "\n")
        if (!is.null(proj$role)) {
            md <- paste0(md, "**Role**: ", proj$role, "\n\n")
        }
        if (!is.null(proj$description)) {
            md <- paste0(md, proj$description, "\n\n")
        }
    }

    # Teaching
    md <- paste0(md, "## Teaching\n\n")
    for (teach in data$teaching) {
        md <- paste0(md, "### ", teach$institution, "\n\n")
        for (role in teach$roles) {
            md <- paste0(md, "- **", role$title, "**\n")
            for (course in role$courses) {
                term <- names(course)[1]
                name <- course[[1]]
                md <- paste0(md, "  - ", term, ": ", name, "\n")
            }
        }
        md <- paste0(md, "\n")
    }

    # Talks & Posters
    md <- paste0(md, "## Talks & Posters\n\n")
    md <- paste0(md, "### Talks\n\n", gen_md_pubs(pubs$talks), "\n")
    md <- paste0(
        md,
        "### Posters (Refereed)\n\n",
        gen_md_pubs(pubs$posters),
        "\n"
    )
    md <- paste0(md, "### Tutorials\n\n", gen_md_pubs(pubs$tutorials), "\n")

    # Skills
    md <- paste0(md, "## Skills\n\n")
    if (!is.null(data$skills)) {
        md <- paste0(
            md,
            "**Languages**: ",
            paste(data$skills$languages, collapse = ", "),
            "\n\n"
        )
        md <- paste0(
            md,
            "**Tools**: ",
            paste(data$skills$tools, collapse = ", "),
            "\n\n"
        )
    }

    # Misc (Service/Grants)
    md <- paste0(md, "## Grants and Scholarships\n\n")
    md <- paste0(md, gen_md_list(data$grants), "\n")

    md <- paste0(md, "## Service\n\n")
    if (!is.null(data$service)) {
        for (svc in data$service) {
            md <- paste0(md, "### ", svc$group, "\n\n")
            md <- paste0(md, gen_md_list(svc$items), "\n")
        }
    }

    return(md)
}

writeLines(content, output_file)
cat("Generated", output_file, "\n")

writeLines(gen_markdown_cv(), "index.md")
cat("Generated index.md\n")
