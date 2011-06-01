# Welcome to configurator

This is a library for configuring Haskell daemons and programs.

Its features include:

* Automatic, dynamic reloading in response to modifications to
  configuration files.

* A simple, but flexible, configuration language, supporting several
  of the most commonly needed types of data, along with interpolation
  of strings from the configuration or the system environment
  (e.g. `$(HOME)`).

* Subscription-based notification of changes to configuration
  properties.

* An `import` directive allows the configuration of a complex
  application to be split across several smaller files, or
  configuration data to be shared across several applications.

# Configuration file format

For details of the configuration file format, see [the Haddock documentation](http://hackage.haskell.org/packages/archive/configurator/latest/doc/html/Data-Configurator.html).

# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/mailrank/configurator/issues).

Master [git repository](http://github.com/mailrank/configurator):

* `git clone git://github.com/mailrank/configurator.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/configurator):

* `hg clone http://bitbucket.org/bos/configurator`

(You can create and contribute changes using either git or Mercurial.)

Authors
-------

This library is written and maintained by Bryan O'Sullivan,
<bos@mailrank.com>.
