# Minimal CV Generator

This folder contains a minimal, self-contained system for generating your academic CV in both PDF (via LaTeX) and Markdown (for Jekyll/HTML) formats.

## Files

*   **`cv.yaml`**: The single source of truth for your CV data. Edit this file to update your information.
*   **`generate_cv.R`**: The R script that reads `cv.yaml` and generates the output files. It contains embedded LaTeX templates and logic.
*   **`cv_complete.pdf`**: The generated PDF CV.
*   **`index.md`**: The generated Markdown CV (serves as the homepage/index for Jekkyl).

## How to Usage

### Prerequisites

*   **R**: You need to have R installed.
*   **LaTeX environment**: You need `pdflatex` and standard packages (like `texlive-full` or `mactex`).
*   **R Packages**: Install `yaml` and `stringr` if you haven't already:
    ```r
    install.packages(c("yaml", "stringr"))
    ```

### Generating the CV

Run the following command in your terminal from inside this directory:

```bash
Rscript generate_cv.R && pdflatex cv_complete.tex
```

This will:
1.  Read `cv.yaml`.
2.  Generate `cv_complete.tex` (LaTeX) and `index.md` (Markdown).
3.  Compile `cv_complete.tex` into `cv_complete.pdf`.

## For Jekyll / GitHub Pages

The script generates a `index.md` file. You can copy this file to your Jekyll site's directory (e.g., as `index.md` or into `_pages/`). It includes standard YAML Front Matter.
