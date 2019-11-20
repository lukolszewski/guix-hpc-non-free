Proprietary HPC Software for GNU Guix
=========================================

This repository contains [GNU Guix](https://gnu.org/s/guix) packages for
high-performance computing (HPC), specifically [_non-free
software_](https://www.gnu.org/philosophy/free-software-even-more-important.html),
or free software that depends on non-free software such as extensions of
[Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc) packages.

This is provided for convenience to HPC researchers and practitioners.
However, note that non-free software denies users the ability to study
and modify it.  Thus, these packages are detrimental to user freedom and
to proper scientific review and experimentation.  As such, we kindly
invite you not to share it widely.

## How does it work?

The package recipes in this repo _extend_ [those that come with
Guix](https://gnu.org/s/guix/packages) and
[Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc).  To make them
visible to the `guix` command-line tools, and assuming you’re using
Guix >= 0.16.0, create a `~/.config/guix/channels.scm` file with the
following snippet:

```
(cons (channel
        (name 'guix-hpc-non-free)
        (url "https://gitlab.inria.fr/guix-hpc/guix-hpc-non-free.git"))
      %default-channels)
```

That way, `guix pull` will systematically pull not only Guix, but also
Guix-HPC.

See [Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc), for more
information.
