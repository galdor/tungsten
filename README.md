# Introduction
Tungsten is a Common Lisp toolkit providing a wide range of features. It is
developped as the foundation for server software and associated tools.

It currently runs on either [SBCL](http://sbcl.org/) or
[CCL](https://ccl.clozure.com/). I might port it to other implementations in
the future.

Tungsten does not have any dependency. The only external component is
[ASDF](https://asdf.common-lisp.dev/) which is used for system management.

# Versioning
Tungsten is currently in development. Anything can change at any moment
without any notice.

**You should not use Tungsten in production.**

The stabilization process will occur later and will result in a standard
system of stable releases.

# Licensing
Tungsten is open source software distributed under the
[ISC](https://opensource.org/licenses/ISC) license.

# Contributions
## Open source, not open contribution
[Similar to SQLite](https://www.sqlite.org/copyright.html), Tungsten is open
source but not open contribution for multiple reasons:

- It avoid potential intellectual property and licensing issues.
- It removes the burden of reviewing patches and maintaining the resulting
  code.
- It helps keeping the software focused on a clear vision.

While this might be disappointing to you, this choice helps me continue to
build and maintain Tungsten.

## Bug reporting
I am thankful for any bug report. Feel free to open issues and include as much
useful information as possible. I cannot however guarantee that I will fix
every bug.

Commercial support will be offered once a first stable version has been
finished.

## Ideas and feature suggestions
Ideas about current systems and suggestions for new ones are welcome, either
on GitHub discussions or by [email](mailto:nicolas@n16f.net).

You can also [hire me](mailto:nicolas@exograd.com) to develop specific
features.
