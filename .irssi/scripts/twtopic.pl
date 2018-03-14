# John Engelbrecht's original versions (<= 1.0.2) were released under the public
# domain. Ryan Kavanagh's changes are distributed under the ISC license:
#
# Copyright (C) 2017 Ryan Kavanagh <rak@ryanak.ca>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

use strict;
use vars qw($VERSION %IRSSI);
use Irssi;
use Irssi::Irc;
use Irssi::TextUI;

$VERSION = '2.0';
%IRSSI   = (
    authors     => 'John Engelbrecht, Ryan Kavanagh',
    contact     => 'jengelbr@yahoo.com, rak@ryanak.ca',
    name        => 'twtopic.pl',
    description => 'Animated Topic bar.',
    sbitems     => 'twtopic',
    license     => 'Public Domain, ISC',
    changed     => 'Wed Jan 03 16:20:17 EST 2017',
    url         => 'http://irssi.darktalker.net' . "\n",
);

my $instrut =
    ".--------------------------------------------------.\n"
  . "| 1.) shell> mkdir ~/.irssi/scripts                |\n"
  . "| 2.) shell> cp twtopic.pl ~/.irssi/scripts/       |\n"
  . "| 3.) shell> mkdir ~/.irssi/scripts/autorun        |\n"
  . "| 4.) shell> ln -s ~/.irssi/scripts/twtopic.pl \\   |\n"
  . "|            ~/.irssi/scripts/autorun/twtopic.pl   |\n"
  . "| 5.) /sbar topic remove topic                     |\n"
  . "| 6.) /sbar topic remove topic_empty               |\n"
  . "| 7.) /sbar topic add -after topicbarstart         |\n"
  . "|        -priority 100 -alignment left twtopic     |\n"
  . "| 9.) /toggle twtopic_instruct and last /save      |\n"
  . "|--------------------------------------------------|\n"
  . "|  Options:                               Default: |\n"
  . "|  /set twtopic_refresh <speed>              150   |\n"
  . "|  /set twtopic_size <size>                  20    |\n"
  . "|  /set twtopic_padding <size>               20    |\n"
  . "|  /set twtopic_auto_resize <ON|OFF>         OFF   |\n"
  . "|  /set twtopic_ar_padding <size>            0     |\n"
  . "|  /set twtopic_min_scroll <ON|OFF>          OFF   |\n"
  . "|  /set twtopic_init_pause <length>          10    |\n"
  . "|  /toggle twtopic_instruct |Startup instructions  |\n"
  . "\`--------------------------------------------------'";

my $timeout;
my $start_pos = 0;
my $size;
my $min_scroll;
my $padding;
my $topic          = "";
my @mirc_color_arr = (
    "\0031",  "\0035",  "\0033",  "\0037",  "\0032", "\0036",
    "\00310", "\0030",  "\00314", "\0034",  "\0039", "\0038",
    "\00312", "\00313", "\00311", "\00315", "\017"
);

sub setup {
    my $window = Irssi::active_win;
    if ( Irssi::settings_get_bool('twtopic_auto_resize') ) {
        $size =
          $window->{'width'} - Irssi::settings_get_int('twtopic_ar_padding');
    }
    else {
        $size = Irssi::settings_get_int('twtopic_size');
    }
    # Subtract 4 because we wrap the topic in '[ ' / ' ]'.
    $size       = $size - 4;
    $min_scroll = Irssi::settings_get_bool('twtopic_min_scroll');
    $padding    = Irssi::settings_get_int('twtopic_padding');
    update_topic();
}

sub regular_timer {
    my $time   = Irssi::settings_get_int('twtopic_refresh');
    Irssi::timeout_remove($timeout);
    $timeout = Irssi::timeout_add( $time, 'reload', undef );
}

sub init_timer {
    my $time   = Irssi::settings_get_int('twtopic_init_pause');
    Irssi::timeout_remove($timeout);
    $timeout = Irssi::timeout_add( $time, 'regular_timer', undef );
}

sub show {
    my ( $item, $get_size_only ) = @_;
    my $text = get();
    $text = "[ " . $text . " ]";
    $item->default_handler( $get_size_only, $text, undef, 1 );
}

sub update_topic {
    $topic = "";
    my $name = Irssi::active_win()->{active}->{name};
    my $type = Irssi::active_win()->{active}->{type};
    if ( $name eq "" ) {
        # We're in the status window
        $topic =
            "Irssi website: http://www.irssi.org, "
            . "Irssi IRC channel: #irssi @ irc://irc.freenode:6667, "
            . "twtopic has been written by Tech Wizard and ryanakca";
    }
    elsif ( $type eq "QUERY" ) {
        $topic = "You are now talking to...... " . $name;
    }
    else {
        my $channel = Irssi::Irc::Server->channel_find($name);
        $topic = $channel->{topic};
        foreach (@mirc_color_arr) { $topic =~ s/$_//g; }
    }
    if ( $topic eq "" ) {
        $topic = "=-=-=-=-= No Topic =-=-=-=-=-=-=-";
    }
    $topic =~ s/(\00313)+//;
    $topic =~ s/(\002)+//;
    $topic =~ s/(\001)+//;
    # Reset the topic and pause
    init_timer();
    # The length of the topic may have changed. We should reset our $start_pos
    # to avoid going off the end in case the topic got shorter.
    $start_pos = -1;
    reload ();
}

sub get {
    my $topiclen = length($topic);
    if ( $topiclen <= $size && $min_scroll ) {
        return $topic . ( ' ' x ( $size - $topiclen ) );
    }
    my $padded = $topic . ( ' ' x $padding );
    my $str    = "";
    my $needed = $size - length($str);
    while ( $needed > 0 ) {
        if ( $needed < length($padded) - $start_pos ) {
            $str = $str . substr( $padded, $start_pos, $needed );
        }
        elsif ( $needed < length($padded) ) {
            $str =
                $str
              . substr( $padded, $start_pos )
              . substr( $padded, 0, $needed - length($padded) );
        }
        else {
            $str =
                $str
              . substr( $padded, $start_pos )
              . substr( $padded, 0, $start_pos );
        }
        $needed = $size - length($str);
    }
    $start_pos = $start_pos + 1 < length($padded) ? $start_pos + 1 : 0;
    return $str;
}


Irssi::signal_add( 'channel topic changed', 'update_topic' );
Irssi::signal_add( 'setup changed',         'setup' );
Irssi::signal_add( 'terminal resized',      'setup' );
Irssi::signal_add( 'window changed',        'update_topic' );

Irssi::settings_add_bool( 'tech_addon', 'twtopic_auto_resize', 0 );
Irssi::settings_add_bool( 'tech_addon', 'twtopic_instruct',    1 );
Irssi::settings_add_bool( 'tech_addon', 'twtopic_min_scroll',  0 );
Irssi::settings_add_int( 'tech_addon', 'twtopic_ar_padding', 10 );
Irssi::settings_add_int( 'tech_addon', 'twtopic_init_pause', 10 );
Irssi::settings_add_int( 'tech_addon', 'twtopic_padding',    20 );
Irssi::settings_add_int( 'tech_addon', 'twtopic_refresh',    150 );
Irssi::settings_add_int( 'tech_addon', 'twtopic_size',       20 );

sub reload { Irssi::statusbar_items_redraw('twtopic'); }

if ( Irssi::settings_get_bool('twtopic_instruct') ) {
    print $instrut;
}

setup();

Irssi::statusbar_item_register( 'twtopic', '$0', 'show' );
