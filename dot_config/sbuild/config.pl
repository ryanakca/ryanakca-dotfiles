# example for ~/.sbuildrc.  (Also see /etc/sbuild/sbuild.conf.)  -*- Perl -*-
#
# Default settings are commented out.
# Additional options found in /etc/sbuild/sbuild.conf may be
# overridden here.


##
## DPKG-BUILDPACKAGE OPTIONS
##

# Name to use as override in .changes files for the Maintainer: field
# Defaults to the DEBEMAIL environment variable, if set, or else the
# Maintainer: field will not be overridden unless set here.
#$maintainer_name='Francesco Paolo Lovergine <frankie@debian.org>';

# Name to use as override in .changes file for the Changed-By: field.
#$uploader_name='Francesco Paolo Lovergine <frankie@debian.org>';

# Key ID to use in .changes for the current upload.
# It overrides both $maintainer_name and $uploader_name
#$key_id='Francesco Paolo Lovergine <frankie@debian.org>';

# PGP-related option to pass to dpkg-buildpackage. Usually neither .dsc
# nor .changes files shall be signed automatically.
#$pgp_options = ['-us', '-uc'];

# By default, do not build a source package (binary only build).
# Set to 1 to force creation of a source package, but note that
# this is inappropriate for binary NMUs, where the option will
# always be disabled.
#$build_source = 0;

# By default, the -s option only includes the .orig.tar.gz when needed
# (i.e. when the Debian revision is 0 or 1).  By setting this option
# to 1, the .orig.tar.gz will always be included when -s is used.
# This is equivalent to --force-orig-source.
#$force_orig_source = 0;

# PATH to set when running dpkg-buildpackage.
#$path = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/X11R6/bin:/usr/games";

# This command is run with the dpkg-buildpackage command line passed to it
# (in the chroot, if doing a chrooted build).  It is used by the sparc buildd
# (which is sparc64) to call the wrapper script that sets the environment to
# sparc (32-bit).  It could be used for other build environment setup scripts
#
#$build_env_cmnd = "";

$build_arch_all = 1;

$run_lintian = 1;
$lintian_opts = ['-i', '-I', '-v', '-E'];

##
## SBUILD BEHAVIOUR
##

# Default distribution.  By default, no distribution is defined, and
# the user must specify it with the -d option.  However, a default may
# be configured here if desired.  Users must take care not to upload
# to the wrong distribution when this option is set, for example
# experimental packages will be built for upload to unstable when this
# is not what is required.
$distribution = 'unstable';

# Default chroot (defaults to distribution[-arch][-sbuild])
$chroot = 'chroot:unstable-amd64';

# When to purge the build directory afterwards; possible values are "never",
# "successful", and "always"
#$purge_build_directory="successful";
# We use LVM snapshots, no need
$purge_build_directory="never";

# sbuild behaviour; possible values are "user" (exit status reports
# build failures) and "buildd" (exit status does not report build
# failures) for use in a buildd setup.
#$sbuild_mode = "user";


##
## TIMEOUTS
##

# Time to wait for a source dependency lock.  The default is 1 minute.
#$srcdep_lock_wait = 1; # 1 minute

# Time (in minutes) of inactivity after which a build is terminated. Activity
# is measured by output to the log file.
#$stalled_pkg_timeout = 150;

# Some packages may exceed the general timeout (e.g. redirecting output to
# a file) and need a different timeout. Below are some examples.
#%individual_stalled_pkg_timeout = (smalleiffel => 300,
#				   jade => 300,
#				   atlas => 300,
#				   glibc => 1000,
#				   'gcc-3.3' => 300,
#				   kwave => 600);
#

##
## FILE AND DIRECTORY LOCATIONS
##

# This option is deprecated.  Directory for chroot symlinks and sbuild
# logs.  Defaults to the current directory if unspecified.  It is used
# as the location of chroot symlinks (obsolete) and for current build
# log symlinks and some build logs.  There is no default; if unset, it
# defaults to the current working directory.  $HOME/build is another
# common configuration.
#$build_dir = undef;

# Directory for writing build logs to
$log_dir = "$HOME/tmp/sbuild/logs";

# Directory for writing build statistics to
$stats_dir = "$HOME/tmp/sbuild/stats";

$chroot_mode = "schroot";
$schroot = "schroot";

# don't remove this, Perl needs it:
1;

