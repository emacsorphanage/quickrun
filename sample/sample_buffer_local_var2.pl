#!perl
use strict;
use warnings;

binmode STDOUT, ":utf8";

for my $arg (@ARGV) {
    if (looks_like_number($arg) && $arg == 72) {
        print "くっ\n";
    }
}

# Local Variables:
# quickrun-command-option:   "-MScalar::Util=looks_like_number -Mutf8"
# quickrun-command-argument: "765 IdleM@ster 72 "
# End:
