# HS-Julia

**HS-Julia** is a Haskell-based application designed to visualize the Julia set of quadratic complex polynomials.

## **🎥 Demos**

![Julia sets](/assets/HS-Julia.gif)

## 📋 Table of Contents
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [How to Run](#how-to-run)
  - [Using Docker (Recommended)](#using-docker)
  - [Using Cabal](#using-cabal-recommended)
- [File Structure](#file-structure)
- [License](#license)

## 🛠 Prerequisites

To run this application locally, you will need the following installed on your machine:

* **Docker**: It is recommended to use [Docker](https://www.docker.com/products/docker-desktop/) to run the application in a containerized environment.
* **Haskell Toolchain**: It is recommended to use [GHCup](https://www.haskell.org/ghcup/) to install GHC (Glasgow Haskell Compiler) and Cabal (Common Architecture for Building Applications and Libraries).
* **Git**: To clone the repository.

## 📥 Installation

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/JunghunLeePhD/HS-Julia.git](https://github.com/JunghunLeePhD/HS-Julia.git)
    cd HS-Julia
    ```

## 🚀 How to Run

You can run the program using either Docker or Cabal directly.

### Using Docker (Recommended)

The repository includes a `Dockerfile`, allowing you to build and run the application without installing Haskell locally.

1.  **Build the Docker image:**
    ```bash
    docker build -t hs-julia .
    ```

2.  **Run the container:**
    ```bash
    docker run -it --rm -v "$(pwd):/export" hs-julia
    ```

### Using Cabal 

1.  **Update your package list:**
    ```bash
    cabal update
    ```

2.  **Build the project:**
    This command will install all dependencies specified in `Julia.cabal` and compile the source code.
    ```bash
    cabal build
    ```

3.  **Run the application for `output/*.ppm` files:**
    ```bash
    cabal run
    ```

4. **Run ffmpeg for `julia_movie.mp4` file:**
    ```bash
    ffmpeg -framerate 30 -i output/frame_%04d.ppm -c:v libx264 -pix_fmt yuv420p -y julia_movie.mp4
    ```

## 📂 File Structure

The project is organized as follows:

```text
HS-Julia/
├── src/                # Source code files (e.g., Main.hs, Lib.hs)
├── Julia.cabal         # Project configuration, metadata, and dependencies
├── Dockerfile          # Configuration for building the Docker image
├── CHANGELOG.md        # History of changes and versions
├── .gitignore          # Files ignored by Git
└── README.md           # Project documentation
```






- **src/**: Contains the core logic for the Julia set visualization.


- **Julia.cabal**: The package description file used by Cabal. It lists the libraries required to build the project.


- **Dockerfile**: Defines the environment to run the application in a container.


## **📝 Usage**

*Check the source code in `*src/*` (specifically `*Main.hs*`) to see if the program accepts command-line arguments (e.g., coordinates, resolution, or output filenames).*

If the program outputs an image file, it will typically be saved in the root directory or the location specified in the code.

## 🤝 Contributing

[](https://github.com/JunghunLeePhD/CPP-Zeta#-contributing)

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the Project


2. Create your Feature Branch (`git checkout -b feature/NEWFEATURE`)


3. Commit your Changes (`git commit -m 'Add NEWFEATURE'`)


4. Push to the Branch (`git push origin feature/NEWFEATURE`)


5. Open a Pull Request

## 📄 License

[](https://github.com/JunghunLeePhD/HS-Julia#-license)

Distributed under the MIT License. See `LICENSE` for more information.

Created by [*Junghun Lee, PhD*](https://github.com/JunghunLeePhD)