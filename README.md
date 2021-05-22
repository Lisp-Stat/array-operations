
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/array-operations">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Array Operations</h3>

  <p align="center">
  A collection of functions and macros for manipulating Lisp-Stat data-frames and CL arrays
	<br />
    <a href="https://lisp-stat.dev/docs/tasks/array-operations/"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/bendudson/array-operations/issues">Report Bug</a>
    ·
    <a href="https://github.com/bendudson/array-operations/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/array-operations/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license.md">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

The `array-operations` system is a collection of functions and
macros for manipulating Common Lisp arrays and performing numerical
calculations with them.

Array-operations is a 'generic' way of operating on array like data
structures. Several `aops` functions have been implemented for
`data-frame`.  For those that haven't, you can transform arrays to
data frames using the `df:matrix-df` function, and a data-frame to an
array using `df:as-array`.  This make it convenient to work with the
data sets using either system.

This repository tracks the [canonical upstream
repo](https://github.com/bendudson/array-operations), with minor
modifications and additional documentation for Lisp-Stat.



### Built With

* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [array-operations (upstream)](https://github.com/bendudson/array-operations)


<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](https://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

### Quicklisp Installation

```lisp
(ql:quickload :array-operations)
```

### Manual Installation

1. Clone the repository
   ```sh
   cd ~/quicklisp/local-projects &&
   git clone https://github.com/Lisp-Stat/array-operations.git
   ```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (ql:quickload :array-operations)
   ```

<!-- USAGE EXAMPLES -->
## Usage

Arrays can be created with numbers from a statistical distribution:

```lisp
(rand '(2 2)) ; => #2A((0.62944734 0.2709539) (0.81158376 0.6700171))
```

in linear ranges:

```lisp
(linspace 1 10 7) ; => #(1 5/2 4 11/2 7 17/2 10)
```

or generated using a function, optionally given index position

`(generate #'identity '(2 3) :position) ; => #2A((0 1 2)
	                                             (3 4 5))`

For more examples, please refer to the [manual](https://lisp-stat.dev/docs/tasks/array-operations).


<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/bendudson/array-operations/issues) for a list of proposed features (and known issues).

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/)
project; that should be your first stop for information.  Also see the
[resources](https://lisp-stat.dev/docs/resources) and
[community](https://lisp-stat.dev/community) page for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create.  Any contributions you make are greatly appreciated.  Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MIT License. See [LICENSE](LICENSE.md) for more information.



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/array-operations](https://github.com/lisp-stat/array-operations)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/array-operations.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/array-operations/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/bendudson/array-operations.svg?style=for-the-badge
[forks-url]: https://github.com/bendudson/array-operations/network/members
[stars-shield]: https://img.shields.io/github/stars/bendudson/array-operations.svg?style=for-the-badge
[stars-url]: https://github.com/bendudson/array-operations/stargazers
[issues-shield]: https://img.shields.io/github/issues/bendudson/array-operations.svg?style=for-the-badge
[issues-url]: https://github.com/bendudson/array-operations/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/array-operations.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/array-operations/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/


