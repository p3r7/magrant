# magrant


An attempt of a Vagrant porcelain inside Emacs.

Mostly an excuse to play around with [transient](https://github.com/magit/transient).

Heavily inspired by packages:

 - [magit](https://github.com/magit/magit) (for git)
 - [kubel](https://github.com/abrochard/kubel) (for Kubernetes)
 - [docker](https://github.com/Silex/docker.el) (for Docker)


## Why this name?!

There was already a [vagrant](https://melpa.org/#/vagrant) package on Melpa.

But it's not as feature-full as this one and doesn't use transient.

So, in lack of inspiration, I called it `magrant`, as in `magit for vagrant`.


## Installation

Not yet on [Melpa](https://melpa.org/).

With `use-package` + `quelpa` + `quelpa-use-package`:

```el
(use-package magrant
  :quelpa (magrant :fetcher github :repo "p3r7/magrant"))
```


## Usage

Just call command `magrant` or directly `magrant-boxes` or `magrant-machines`.

You can then manage your machines and boxes like you would in the command line.

All action work on any number of entry. So you could for example start all your machines at once or SSH into half of them.


## Legibility

This code uses form feeds (`^L` character) as separators.

Package [form-feed](https://github.com/wasamasa/form-feed) makes them appear as intended.
