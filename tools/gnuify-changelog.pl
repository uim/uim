#!/usr/bin/perl -w

# a script to munge the output of 'svn log' into something approaching the 
# style of a GNU ChangeLog.
#
# to use this, just fill in the 'hackers' hash with the usernames and 
# name/emails of the people who work on your project, go to the top level 
# of your working copy, and run:
#
# $ svn log | /path/to/gnuify-changelog.pl > ChangeLog

%hackers = ( "tkng"     => 'TOKUNAGA Hiroyuki <tkng@xem.jp>',
	     "yusuke"   => 'Yusuke TABATA <yusuke@w5.dion.ne.jp>',
	     "yamaken"  => 'YAMAMOTO Kengo / YamaKen <yamaken@bp.iij4u.or.jp>',
	     "omote"    => 'Masahito Omote <omote@utyuuzin.net>',
	     "kzk"      => 'kzk <mover@hct.zaq.ne.jp>',
	     "makeinu"  => 'Takuro Ashie <ashie@homa.ne.jp>',
	     "ekato"    => 'Etsushi Kato <ek.kato@gmail.com>',
	     "yamamoto" => 'Masanari Yamamoto <h013177b@ice.nuie.nagoya-u.ac.jp>',
	     "jun0"     => 'Jun Inoue <jun.lambda@gmail.com>',
	     "nosuke"   => 'Konosuke Watanabe <sasugaanija@gmail.com>',
	     "jhpark"   => 'Jae-hyeon Park <jhyeon@gmail.com>',
);

$parse_next_line = 0;

while (<>) {
  # axe windows style line endings, since we should try to be consistent, and 
  # the repos has both styles in it's log entries.
  $_ =~ s/\r\n$/\n/;

  if (/^-+$/) {
    # we're at the start of a log entry, so we need to parse the next line
    $parse_next_line = 1;
  } elsif ($parse_next_line) {
    # transform from svn style to GNU style
    $parse_next_line = 0;

    @parts = split (/ /, $_);
    print "$parts[4] $hackers{$parts[2]}\n";
  } else {
    print "\t$_";
  }
}
