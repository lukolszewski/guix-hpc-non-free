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

        ;; Downloading directly from
        ;; <https://gitlab.inria.fr/guix-hpc/guix-hpc-non-free.git> would
        ;; would require authentication, so use a local checkout instead.
        (url (string-append "file://" (getenv "HOME")
		                    "/src/guix-hpc-non-free")))
      %default-channels)
```

This assumes that you have first arranged to clone this very repo under
`~/src`:

```
mkdir -p src
cd src
git clone https://gitlab.inria.fr/guix-hpc/guix-hpc-non-free.git
```

That way, `guix pull` will systematically pull not only Guix, but also
Guix-HPC.

If you’re using a version of Guix older than 0.16.0, you can instead
manually check out this repository and drop it in `GUIX_PACKAGE_PATH`:

```
# We assume https://gitlab.inria.fr/guix-hpc/guix-hpc is already
# in your GUIX_PACKAGE_PATH.

$ git clone https://gitlab.inria.fr/guix-hpc/guix-hpc-non-free.git
$ export GUIX_PACKAGE_PATH=$PWD/guix-hpc-non-free:$GUIX_PACKAGE_PATH
$ guix package -i starpu-cuda
The following package will be installed:
   starpu-cuda	1.2.1	/gnu/store/rkbcfj1prdn5i0ama1qli5zw37ajv1ac-starpu-cuda-1.2.1

The following derivations will be built:
   /gnu/store/8ppai259g8xjzk42q3pygjfaqin2b29n-profile.drv
   /gnu/store/xv5r6sxybz441jfgzn0skj7gm2p37dfa-starpu-cuda-1.2.1.drv
```

See [Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc), for more
information.
