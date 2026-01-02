# Minimal CV Generator

This folder contains a minimal, self-contained system for generating your academic CV in both PDF (via LaTeX) and Markdown (for Jekyll/HTML) formats.

## Files

*   **`cv.yaml`**: The single source of truth for your CV data. Edit this file to update your information.
*   **`generate_cv.R`**: The R script that reads `cv.yaml` and generates the output files. It contains embedded LaTeX templates and logic.
*   **`cv_complete.pdf`**: The generated PDF CV.
*   **`cv.md`**: The generated Markdown CV (suitable for Jekyll/GitHub Pages).

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
2.  Generate `cv_complete.tex` (LaTeX) and `cv.md` (Markdown).
3.  Compile `cv_complete.tex` into `cv_complete.pdf`.

## For Jekyll / GitHub Pages

The script generates a `cv.md` file. You can copy this file to your Jekyll site's directory (e.g., as `cv.md` or into `_pages/`). It includes standard YAML Front Matter.

## Automation (GitHub Actions)

This repository includes a GitHub Actions workflow (`.github/workflows/build_cv.yml`) that automatically rebuilds your CV whenever you push changes to the `cv_generator` folder.

It will:
1.  Install R and LaTeX dependencies.
2.  Run the generator.
3.  Commit and push the updated `cv_complete.pdf` and `cv.md` files back to your repository.

You don't need to manually run the scripts if you are using GitHub; just edit `cv.yaml`, push, and let the Action handle the rest.
