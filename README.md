Proprietary HPC Software for GNU Guix
=========================================

[![pipeline status](https://guix.bordeaux.inria.fr/jobset/guix-hpc-non-free/badge.svg)](https://guix.bordeaux.inria.fr/jobset/guix-hpc-non-free)

This repository contains [GNU Guix](https://gnu.org/s/guix) packages for
high-performance computing (HPC), specifically [_non-free
software_](https://www.gnu.org/philosophy/free-software-even-more-important.html),
or free software that depends on non-free software such as extensions of
[Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc) packages.

## Important note

This is provided for convenience to HPC researchers and practitioners. Please note this repository contains software considered non-free and as such it is out of scope of support provided for Guix via the default IRC channel, the Guix mailing lists and other official GNU channels etc. Alternative resources to obtain support for running non-free software on Guix exist. The main one is the #nonguix irc channel on Libera.Chat IRC.

This is my working copy of the repository that originates from https://gitlab.inria.fr/guix-hpc/guix-hpc-non-free
If you're interested in using my latest additions such as **pytorch v1.12 with cuda 1.6, MKL 2022.1.0 and cudnn with guix** configure your channels for the luks_changes branch. Due to the unconventional way install files have to be downloaded in advance by the user I'm not submitting those changes upstream nor I would encourage anyone to use my code. However for anyone who wants to use it, is free to do so. I show how to do this below. 

Please note due to Nvidia and Intel blocking unauthenticated downloads of Cuda and MKL this code will fail on your machine, because it expects cuda, cudnn and MLK installers to be present on a local path. If you want to get it working as-is and you've downloaded install files from NVidia and Intel you can make it work by creating the following folders and placing install files in them:
- For cuda 11.6:
/media/Data/software/cuda_11.6.0_510.39.01_linux.run
- For cudnn 8.4.1:
/media/Data/software/cudnn-linux-x86_64-8.4.1.50_cuda11.6-archive.tar.xz
- For MKL 2022.1.0:
/media/Data/software/l_onemkl_p_2022.1.0.223_offline.sh

At the time of this update download links available are shown below. Please note they are likely to be changed and if you're reading this some months later it is unlikely they'll work. If you need those specific versions of the software (and at the end of the day we use guix for repeatability) I suggest to search for the files using your favourite search engine and then compare the hash with the 'guix hash filename' command for some measure of security.

- The link to download CUDA 11.6 after signing up and logging in with NVidia is: https://developer.nvidia.com/cuda-11-6-0-download-archive
- Same as above for CUDNN 8.4.1 is: https://developer.nvidia.com/compute/cudnn/secure/8.4.1/local_installers/11.6/cudnn-linux-x86_64-8.4.1.50_cuda11.6-archive.tar.xz
- Finally for Intel MKL 2022.1.0: https://registrationcenter-download.intel.com/akdlm/irc_nas/18721/l_onemkl_p_2022.1.0.223_offline.sh

## How does it work?

The package definitions in this repo _extend_ [those that come with
Guix](https://hpc.guix.info/browse) and in case of pytorch with cuda [those that come with NonGuix](https://gitlab.com/lukolszewski/nonguix) To make them visible to the
`guix` command-line tools, create the `~/.config/guix/channels.scm` file
with the following snippet to request the `guix-hpc-non-free` and 'nonguix' _channels_:

```scheme
(cons (channel
        (name 'guix-hpc-non-free)
        (url "https://github.com/lukolszewski/guix-hpc-non-free.git")
        (branch "luks_changes")
	(introduction
	  (make-channel-introduction 
	    "0c201e457cbd8fc43fb2d8f5803ca7ff7f4caa1f" 
	    (openpgp-fingerprint "F395EC34DD36058DDE47278CE65527E0F996859B"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix.git"))
      (channel
        (name 'guix-hpc)
        (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
	;;Please note the line below is here just temporarily, it freezes guix-hpc channel
	;;at a certain commit. Unfortunately the next commit after this one introduces
	;;an issue that causes guix pull to fail for me. Therefore until it is resolved
	;;I recommend to use the below.
	(commit "aaa65038bebc947b3deae42acfc83b6f2e972511")
	)
      %default-channels)
```

That way, `guix pull` will systematically pull not only Guix, but also
Guix-HPC-non-free, Guix-HPC and nonguix.

My pytorch 1.12 with cuda 11.6, cudnn and MKL2022.1.0 package depends on nvidia driver version 515.65.1 in the nonguix branch. As of now it is availeble in the nonguix channel. Should nonguix become updated in a way that breaks this dependency one can add the following extra line in their channels.scm just below (name 'nonguix) to get the old version of the channel: (commit "354b152ce2e0819e5b934009e9ae1d87c6ae93e1") where the commit id it the latest commit that contains nvidia-libs that use driver version 515.65.1. Alternatively you can use my branch of nonguix by this channel spec:

```scheme
       (channel
        (name 'nonguix)
        (url "https://gitlab.com/lukolszewski/nonguix.git")
	(branch "luks_changes")
        (introduction
         (make-channel-introduction
          "11d2753ab2d353195afb531e23cacad45298c42d"
          (openpgp-fingerprint
           "F395EC34DD36058DDE47278CE65527E0F996859B"))))
```

See [Guix-HPC](https://gitlab.inria.fr/guix-hpc/guix-hpc) and [NonGuix](https://gitlab.com/nonguix/nonguix), for more
information.

## Warning

Please note this is very much just a quick task I did because I needed pytorch with Cuda+MKL on guix. As such it is likely to contain bugs and in general not provide the best user experience possible. 
