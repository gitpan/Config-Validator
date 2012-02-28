#
# ex1: simple use of Config::Validator with Getopt::Long
#
# $ perl ex1.pl --help
# $ perl ex1.pl --src-host foo --dst-host bar
#

use strict;
use warnings;
use Config::Validator;
use Data::Dumper;
use Getopt::Long;

our($Validator, @Options, %Config);

$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

$Validator = Config::Validator->new({
    type => "struct",
    fields => {
	"debug"    => { type => "integer", optional => "true" },
	"dst-port" => { type => "integer", min => 0, max => 65535, optional => "true" },
	"dst-host" => { type => "string", match => qr/^[\w\-\.]+$/ },
	"help"     => { type => "boolean", optional => "true" },
	"src-port" => { type => "integer", min => 0, max => 65535, optional => "true" },
	"src-host" => { type => "string", match => qr/^[\w\-\.]+$/ },
    },
});

@Options = sort($Validator->options());

GetOptions(\%Config, @Options) or die;

if ($Config{help}) {
    printf("Options:%s\n", join("\n  --", "", @Options));
    exit(0);
}

$Validator->validate(\%Config);

print(Dumper(\%Config));
