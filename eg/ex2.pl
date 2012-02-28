#
# ex2: advanced use of Config::Validator with Getopt::Long
#
# compared to ex1:
#  - aliases are given to --debug and --help
#  - --debug can be repeated (Getopt::Long's "+")
#  - named schemas are used to avoid schema duplication
#  - the configuration hash is treeified
#
# $ perl ex2.pl -h
# $ perl ex2.pl -d -d --src-host foo --dst-host bar --dst-port 80
#

use strict;
use warnings;
use Config::Validator qw(treeify);
use Data::Dumper;
use Getopt::Long;

our($Validator, @Options, %Config);

$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

$Validator = Config::Validator->new(
    svc => {
	type => "struct",
	fields => {
	    port => { type => "integer", min => 0, max => 65535, optional => "true" },
	    host => { type => "string", match => qr/^[\w\-\.]+$/ },
	},
    },
    cfg => {
	type => "struct",
	fields => {
	    debug => { type => "integer", optional => "true" },
	    dst   => { type => "valid(svc)" },
	    help  => { type => "boolean", optional => "true" },
	    src   => { type => "valid(svc)" },
	},
    },
);

@Options = sort($Validator->options("cfg"));

foreach my $option (@Options) {
    $option =~ s/^debug.*$/debug|d+/;
    $option =~ s/^help/help|h|?/;
}

GetOptions(\%Config, @Options) or die;

if ($Config{help}) {
    printf("Options:%s\n", join("\n  --", "", @Options));
    exit(0);
}

treeify(\%Config);
$Validator->validate(\%Config, "cfg");

print(Dumper(\%Config));
