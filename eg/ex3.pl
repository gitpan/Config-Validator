#
# ex3: simple use of Config::Validator with Getopt::Long and Config::General
#
# compared to ex1:
#  - the --config option has been added
#  - configuration can come both from a file and from the command line
#
# $ perl ex3.pl --help
# $ perl ex3.pl --src-host foo --dst-host bar
# $ perl ex3.pl --src-host foo --config ex3-cfg1
#

use strict;
use warnings;
use Config::General qw(ParseConfig);
use Config::Validator;
use Data::Dumper;
use Getopt::Long;

our($Validator, @Options, %Config, @Tmp, %Tmp);

$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

$Validator = Config::Validator->new({
    type => "struct",
    fields => {
	"config"   => { type => "string", optional => "true" },
	"debug"    => { type => "integer", optional => "true" },
	"dst-port" => { type => "integer", min => 0, max => 65535, optional => "true" },
	"dst-host" => { type => "string", match => qr/^[\w\-\.]+$/ },
	"help"     => { type => "boolean", optional => "true" },
	"src-port" => { type => "integer", min => 0, max => 65535, optional => "true" },
	"src-host" => { type => "string", match => qr/^[\w\-\.]+$/ },
    },
});

@Options = sort($Validator->options());

# first step: parse the command line options to handle options like --help or --config

@Tmp = @ARGV;
GetOptions(\%Tmp, @Options) or die;
if ($Tmp{help}) {
    printf("Options:%s\n", join("\n  --", "", @Options));
    exit(0);
}

# second step: handle the --config option

if ($Tmp{config}) {
    %Config = ParseConfig(-ConfigFile => $Tmp{config});
    # third step: parse again the command line options using defaults read from the file
    @ARGV = @Tmp;
    GetOptions(\%Config, @Options) or die;
} else {
    %Config = %Tmp;
}

$Validator->validate(\%Config);

print(Dumper(\%Config));
