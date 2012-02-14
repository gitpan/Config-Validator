#+##############################################################################
#                                                                              #
# File: Config/Validator.pm                                                    #
#                                                                              #
# Description: schema based configuration validation                           #
#                                                                              #
#-##############################################################################

#
# module definition
#

package Config::Validator;
use strict;
use warnings;
our $VERSION  = "0.2";
our $REVISION = sprintf("%d.%02d", q$Revision: 1.12 $ =~ /(\d+)\.(\d+)/);

#
# export control
#

use Exporter;
our(@ISA, @EXPORT, @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw(string2hash hash2string is_regexp);

#
# used modules
#

use Scalar::Util qw(blessed reftype);
use URI::Escape qw(uri_escape uri_unescape);

#
# global variables
#

our(
    $_Known,		# hash of known schemas used by _check_type()
    $_BuiltIn,		# hash of built-in schemas used to validate schemas
);

#+++############################################################################
#                                                                              #
# helper functions                                                             #
#                                                                              #
#---############################################################################

#
# report a fatal error with a sprintf() API
#

sub _fatal ($@) {
    my($message, @arguments) = @_;

    $message = sprintf($message, @arguments) if @arguments;
    $message =~ s/\s+$//;
    die(caller() . ": $message\n");
}

#
# stringify any scalar, including undef
#

sub _string ($) {
    my($scalar) = @_;

    return(defined($scalar) ? "$scalar" : "<undef>");
}

#
# test if something is a regular expression
#

if ($] >= 5.010) {
    require re;
    re->import(qw(is_regexp));
} else {
    *is_regexp = sub { return(ref($_[0]) eq "Regexp") };
}

#
# format an error
#

sub _errfmt (@);
sub _errfmt (@) {
    my(@errors) = @_;
    my($string, $error, $tmp);

    return("") unless @errors;
    $string = shift(@errors);
    foreach $error (@errors) {
	$tmp = ref($error) ? _errfmt(@$error) : $error;
	next unless length($tmp);
	$tmp =~ s/^/  /mg;
	$string .= "\n" . $tmp;
    }
    return($string);
}

#+++############################################################################
#                                                                              #
# conversion helper functions                                                  #
#                                                                              #
#---############################################################################

#
# string -> hash
#

sub string2hash ($) {
    my($string) = @_;
    my(%hash, $kv);

    foreach $kv (split(/\s+/, $string)) {
	_fatal("invalid hash key=value: %s", $kv)
	    unless $kv =~ /^([^\=]+)=(.*)$/;
	$hash{uri_unescape($1)} = uri_unescape($2);
    }
    return(%hash) if wantarray();
    return(\%hash);
}

#
# hash -> string
#

sub hash2string (@) {
    my($hash, $key, @kvs);

    if (@_ == 1 and $_[0] and ref($_[0]) eq "HASH") {
	$hash = $_[0];
    } else {
	$hash = { @_ };
    }
    foreach $key (sort(keys(%$hash))) {
	push(@kvs, uri_escape($key) . "=" . uri_escape($hash->{$key}));
    }
    return(join(" ", @kvs));
}

#
# schema -> options
#

sub _options ($$$@);
sub _options ($$$@) {
    my($valid, $schema, $type, @path) = @_;
    my($field, @list);

    $type ||= $schema->{type};
    # terminal
    return(join("-", @path) . "=s") if $type eq "string";
    return(join("-", @path) . "=f") if $type eq "number";
    return(join("-", @path) . "=i") if $type eq "integer";
    return(join("-", @path)) if $type eq "boolean";
    # assumed to come from strings
    return(join("-", @path) . "=s") if $type =~ /^isa\(.+\)$/ or $type eq "table(string)";
    # recursion
    if ($type =~ /^list\?\((.+)\)$/) {
	return(map($_ . "\@", _options($valid, $schema, $1, @path)));
    }
    if ($type =~ /^valid\((.+)\)$/) {
	_fatal("options(): unknown schema: %s", $1) unless $valid->{$1};
	return(_options($valid, $valid->{$1}, undef, @path));
    }
    if ($type eq "struct") {
	foreach $field (keys(%{ $schema->{fields} })) {
	    push(@list, _options($valid, $schema->{fields}{$field}, undef, @path, $field));
	}
	return(@list);
    }
    # unsupported
    _fatal("options(): unsupported type: %s", $type);
}

#+++############################################################################
#                                                                              #
# built-in schemas                                                             #
#                                                                              #
#---############################################################################

#
# check that a type is valid
#

sub _check_type ($$$);
sub _check_type ($$$) {
    my($valid, $schema, $data) = @_;

    return() if $data =~ /^[a-z]+$/;
    return() if $data =~ /^(ref|isa)\(\*\)$/;
    return() if $data =~ /^(ref|isa)\([\w\:]+\)$/;
    return(_check_type($valid, $schema, $2)) if $data =~ /^(list\??|table)\((.+)\)$/;
    if ($data =~ /^valid\((.+)\)$/) {
	return() if $_Known->{$1};
	return("unknown schema: $1");
    }
    return("unexpected type: $data");
}

#
# schema of a "type"
#

$_BuiltIn->{type} = {
    type  => "string",
    match => qr/ ^ ( anything        # really anything
                   | undef           # undef
                   | undefined       #   "
                   | defined         # not undef
                   | string          # any string
                   | boolean         # either 'true' or 'false'
                   | number          # any number
                   | integer         # any integer
                   | reference       # any reference, blessed or not
                   | ref\(\*\)       #   "
                   | blessed         # any blessed reference
                   | object          #   "
                   | isa\(\*\)       #   "
                   | unblessed       # any reference which is not blessed
                   | code            # a code reference (aka ref(CODE))
                   | regexp          # a regular expression (see is_regexp())
                   | list            # an homogeneous list
                   | list\(.+\)      # idem but with the given subtype
                   | list\?\(.+\)    # shortcut: list?(X) means either X or list(X)
                   | table           # an homogeneous table
                   | table\(.+\)     # idem but with the given subtype
                   | struct          # a structure, i.e. a table with known keys
                   | ref\(.+\)       # a reference of the given kind
                   | isa\(.+\)       # an object of the given kind
                   | valid\(.+\)     # something valid according to the named schema
                   ) $ /x,
    check => \&_check_type,
};

#
# check that a schema is valid
#

sub _check_schema ($$$);
sub _check_schema ($$$) {
    my($valid, $schema, $data) = @_;
    my($field);

    $field = "min";
    goto unexpected if defined($data->{$field})
	and not $data->{type} =~ /^(string|number|integer|list.*|table.*)$/;
    $field = "max";
    goto unexpected if defined($data->{$field})
	and not $data->{type} =~ /^(string|number|integer|list.*|table.*)$/;
    $field = "match";
    goto unexpected if defined($data->{$field})
	and not $data->{type} =~ /^(string|table.*)$/;
    $field = "subtype";
    if ($data->{type} =~ /^(list|table)$/) {
	goto missing unless defined($data->{$field});
    } else {
	goto unexpected if defined($data->{$field});
    }
    $field = "fields";
    if ($data->{type} =~ /^(struct)$/) {
	goto missing unless defined($data->{$field});
    } else {
	goto unexpected if defined($data->{$field});
    }
    return();
  unexpected:
    return(sprintf("unexpected schema field for type %s: %s", $data->{type}, $field));
  missing:
    return(sprintf("missing schema field for type %s: %s", $data->{type}, $field));
}

#
# schema of a "schema"
#

# FIXME: add a description field?

$_BuiltIn->{schema} = {
    type   => "struct",
    fields => {
	type     => { type => "list?(valid(type))" },
	subtype  => { type => "valid(schema)",        optional => "true" },
	fields   => { type => "table(valid(schema))", optional => "true" },
	optional => { type => "boolean",              optional => "true" },
	min      => { type => "number",               optional => "true" },
	max      => { type => "number",               optional => "true" },
	match    => { type => "regexp",               optional => "true" },
	check    => { type => "code",                 optional => "true" },
    },
    check => \&_check_schema,
};

#+++############################################################################
#                                                                              #
# validation helpers                                                           #
#                                                                              #
#---############################################################################

#
# validate that a value is within a numerical range
#

sub _validate_range ($$$$) {
    my($what, $value, $min, $max) = @_;

    return(sprintf("%s is not >= %s: %s", $what, $min, $value))
	if defined($min) and not $value >= $min;
    return(sprintf("%s is not <= %s: %s", $what, $max, $value))
	if defined($max) and not $value <= $max;
    return();
}

#
# validate a list of homogeneous elements
#

sub _validate_list ($$$) {
    my($valid, $schema, $data) = @_;
    my(@errors, $tmp, $index, $element);

    @errors = _validate_range("size", scalar(@$data), $schema->{min}, $schema->{max})
	if defined($schema->{min}) or defined($schema->{max});
    return(@errors) if @errors;
    $index = 0;
    foreach $tmp (@$data) {
	$element = $tmp; # preserved outside loop
	@errors = _validate($valid, $schema->{subtype}, $element);
	goto invalid if @errors;
	$index++;
    }
    return();
  invalid:
    return(sprintf("invalid element %d: %s", $index, _string($element)), \@errors);
}

#
# validate a table of homogeneous elements
#

sub _validate_table ($$$) {
    my($valid, $schema, $data) = @_;
    my(@errors, $tmp, $key);

    @errors = _validate_range("size", scalar(keys(%$data)), $schema->{min}, $schema->{max})
	if defined($schema->{min}) or defined($schema->{max});
    return(@errors) if @errors;
    foreach $tmp (keys(%$data)) {
	$key = $tmp; # preserved outside loop
	@errors = (sprintf("key does not match %s: %s", $schema->{match}, $key))
	    if defined($schema->{match}) and not $key =~ $schema->{match};
	goto invalid if @errors;
	@errors = _validate($valid, $schema->{subtype}, $data->{$key});
	goto invalid if @errors;
    }
    return();
  invalid:
    return(sprintf("invalid element %s: %s", $key, _string($data->{$key})), \@errors);
}

#
# validate a struct, i.e. a hash with known fields
#

sub _validate_struct ($$$) {
    my($valid, $schema, $data) = @_;
    my(@errors, $tmp, $key);

    # check the missing fields
    foreach $tmp (keys(%{ $schema->{fields} })) {
	$key = $tmp; # preserved outside loop
	next if exists($data->{$key});
	next if $schema->{fields}{$key}{optional}
	    and $schema->{fields}{$key}{optional} eq "true";
	return(sprintf("missing field: %s", $key));
    }
    # check the existing fields
    foreach $tmp (keys(%$data)) {
	$key = $tmp; # preserved outside loop
	return(sprintf("unexpected field: %s", $key))
	    unless $schema->{fields}{$key};
	@errors = _validate($valid, $schema->{fields}{$key}, $data->{$key});
	goto invalid if @errors;
    }
    return();
  invalid:
    return(sprintf("invalid field %s: %s", $key, _string($data->{$key})), \@errors);
}

#
# validate something using multiple possible types
#

sub _validate_multiple ($$$@) {
    my($valid, $schema, $data, @types) = @_;
    my($type, @errors, %tmpschema, @tmperrors);

    %tmpschema = %$schema;
    foreach $type (@types) {
	$tmpschema{type} = $type;
	@tmperrors = _validate($valid, \%tmpschema, $data);
	return() unless @tmperrors;
	push(@errors, [ @tmperrors ]);
    }
    return(sprintf("invalid data (none of the types could be validated): %s",
		   _string($data)), @errors);
}

#
# validate something
#

sub _validate ($$$);
sub _validate ($$$) {
    my($valid, $schema, $data) = @_;
    my(@errors, $reftype, $blessed, %tmpschema);

    # check multiple types
    if (ref($schema->{type}) eq "ARRAY") {
	return(_validate_multiple($valid, $schema, $data, @{ $schema->{type} }));
    }
    # check list?(X)
    if ($schema->{type} =~ /^list\?\((.+)\)$/) {
	return(_validate_multiple($valid, $schema, $data, $1, "list($1)"));
    }
    # check valid(X)
    if ($schema->{type} =~ /^valid\((.+)\)$/) {
	return(sprintf("unexpected schema: %s", $1)) unless $valid->{$1};
	return(_validate($valid, $valid->{$1}, $data));
    }
    # check anything
    goto good if $schema->{type} eq "anything";
    # check if defined
    if ($schema->{type} =~ /^(undef|undefined)$/) {
	goto invalid if defined($data);
	goto good;
    }
    return(sprintf("invalid %s: <undef>", $schema->{type})) unless defined($data);
    goto good if $schema->{type} eq "defined";
    # check reference type (for non-reference)
    $reftype = reftype($data);
    if ($schema->{type} =~ /^(string|boolean|number|integer)$/) {
	goto invalid if defined($reftype);
	if ($schema->{type} eq "string") {
	    @errors = _validate_range("length", length($data), $schema->{min}, $schema->{max})
		if defined($schema->{min}) or defined($schema->{max});
	    goto invalid if @errors;
	    @errors = (sprintf("value does not match %s: %s", $schema->{match}, $data))
		if defined($schema->{match}) and not $data =~ $schema->{match};
	    goto invalid if @errors;
	} elsif ($schema->{type} eq "boolean") {
	    goto invalid unless $data =~ /^(true|false)$/;
	} elsif ($schema->{type} eq "number") {
	    goto invalid unless $data =~ /^[\+\-]?(?=\d|\.\d)\d*(\.\d*)?([Ee][\+\-]?\d+)?$/;
	    @errors = _validate_range("value", $data, $schema->{min}, $schema->{max})
		if defined($schema->{min}) or defined($schema->{max});
	    goto invalid if @errors;
	} elsif ($schema->{type} eq "integer") {
	    goto invalid unless $data =~ /^[\+\-]?\d+$/;
	    @errors = _validate_range("value", $data, $schema->{min}, $schema->{max})
		if defined($schema->{min}) or defined($schema->{max});
	    goto invalid if @errors;
	} else {
	    return(sprintf("unexpected type: %s", $schema->{type}));
	}
	goto good;
    }
    # check reference type (for reference)
    goto invalid unless defined($reftype);
    goto good if $schema->{type} =~ /^(reference|ref\(\*\))$/;
    $blessed = defined(blessed($data));
    if ($schema->{type} =~ /^(blessed|object|isa\(\*\))$/) {
	goto invalid unless $blessed;
    } elsif ($schema->{type} eq "unblessed") {
	goto invalid if $blessed;
    } elsif ($schema->{type} eq "code") {
	goto invalid unless $reftype eq "CODE";
    } elsif ($schema->{type} eq "regexp") {
	goto invalid unless is_regexp($data);
    } elsif ($schema->{type} eq "list") {
	goto invalid unless $reftype eq "ARRAY";
	@errors = _validate_list($valid, $schema, $data);
	goto invalid if @errors;
    } elsif ($schema->{type} =~ /^list\((.+)\)$/) {
	goto invalid unless $reftype eq "ARRAY";
	%tmpschema = %$schema;
	$tmpschema{subtype} = { type => $1 };
	@errors = _validate_list($valid, \%tmpschema, $data);
	goto invalid if @errors;
    } elsif ($schema->{type} eq "table") {
	goto invalid unless $reftype eq "HASH";
	@errors = _validate_table($valid, $schema, $data);
	goto invalid if @errors;
    } elsif ($schema->{type} =~ /^table\((.+)\)$/) {
	goto invalid unless $reftype eq "HASH";
	%tmpschema = %$schema;
	$tmpschema{subtype} = { type => $1 };
	@errors = _validate_table($valid, \%tmpschema, $data);
	goto invalid if @errors;
    } elsif ($schema->{type} eq "struct") {
	goto invalid unless $reftype eq "HASH";
	@errors = _validate_struct($valid, $schema, $data);
	goto invalid if @errors;
    } elsif ($schema->{type} =~ /^ref\((.+)\)$/) {
	goto invalid unless $reftype eq $1;
    } elsif ($schema->{type} =~ /^isa\((.+)\)$/) {
	goto invalid unless $blessed and $data->isa($1);
    } else {
	return(sprintf("unexpected type: %s", $schema->{type}));
    }
  good:
    @errors = $schema->{check}->($valid, $schema, $data) if $schema->{check};
    return() unless @errors;
  invalid:
    return(sprintf("invalid %s: %s", $schema->{type}, $data), \@errors);
}

#+++############################################################################
#                                                                              #
# object oriented interface                                                    #
#                                                                              #
#---############################################################################

#
# create a validator object
#

sub new : method {
    my($class, $self, @errors);

    $class = shift(@_);
    $self = {};
    # find out which schema(s) to use
    if (@_ == 0) {
	$self->{schema} = $_BuiltIn;
    } elsif (@_ == 1) {
	$self->{schema}{""} = $_[0];
    } elsif (@_ % 2 == 0) {
	$self->{schema} = { @_ };
    } else {
	_fatal("new(): unexpected number of arguments: %d", scalar(@_));
    }
    # validate them
    {
    	local $_Known = $self->{schema};
    	@errors = _validate($_BuiltIn, { type => "table(valid(schema))" }, $self->{schema});
    }
    _fatal("new(): invalid schema: %s", _errfmt(@errors)) if @errors;
    # so far so good!
    bless($self, $class);
    return($self);
}

#
# validate the given data
#

sub validate : method {
    my($self, $data, $schema, @errors);

    $self = shift(@_);
    # find out what to validate against
    if (@_ == 1) {
	$data = shift(@_);
	_fatal("validate(): no default schema") unless $self->{schema}{""};
	$schema = $self->{schema}{""};
    } elsif (@_ == 2) {
	$data = shift(@_);
	$schema = shift(@_);
	_fatal("validate(): unknown schema: %s", $schema) unless $self->{schema}{$schema};
	$schema = $self->{schema}{$schema};
    } else {
	_fatal("validate(): unexpected number of arguments: %d", scalar(@_));
    }
    # validate data
    {
	local $_Known = $self->{schema};
	@errors = _validate($self->{schema}, $schema, $data);
    }
    _fatal("validate(): %s", _errfmt(@errors)) if @errors;
}

#
# convert to options
#

sub options : method {
    my($self, $schema);

    $self = shift(@_);
    # find out which schema to convert to options
    if (@_ == 0) {
	_fatal("options(): no default schema") unless $self->{schema}{""};
	$schema = $self->{schema}{""};
    } elsif (@_ == 1) {
	$schema = shift(@_);
	_fatal("options(): unknown schema: %s", $schema) unless $self->{schema}{$schema};
	$schema = $self->{schema}{$schema};
    } else {
	_fatal("options(): unexpected number of arguments: %d", scalar(@_));
    }
    # convert to options
    return(_options($self->{schema}, $schema, undef));
}

1;

__DATA__

=head1 NAME

Config::Validator - schema based configuration validation

=head1 SYNOPSIS

  use Config::Validator;

  # simple usage
  $validator = Config::Validator->new({ type => "list(integer)" });
  $validator->validate([ 1, 2 ]);   # OK
  $validator->validate([ 1, 2.3 ]); # FAIL
  $validator->validate({ 1, 2 });   # FAIL

  # advanced usage
  $validator = Config::Validator->new(
      octet => {
          type => "integer",
          min  => 0,
          max  => 255,
      },
      color => {
          type   => "struct",
          fields => {
              red   => { type => "valid(octet)" },
              green => { type => "valid(octet)" },
              blue  => { type => "valid(octet)" },
          },
      },
  );
  $validator->validate(
      { red => 23, green => 47,  blue => 6 }, "color"); # OK
  $validator->validate(
      { red => 23, green => 470, blue => 6 }, "color"); # FAIL
  $validator->validate(
      { red => 23, green => 47,  lbue => 6 }, "color"); # FAIL

=head1 DESCRIPTION

This module allows to perform schema based configuration validation.

The idea is to define in a schema what a valid data is. This schema
can be used to create a validator object that can in turn be used to
make sure that some data indeed conforms to the schema.

Although the primary focus is on "configuration" (for instance as
provided by modules like L<Config::General>) and, to a lesser extent,
"options" (for instance as provided by modules like L<Getopt::Long>),
this module can in fact validate any data structure.

=head1 METHODS

The following methods are available:

=over

=item new([OPTIONS])

return a new Config::Validator object (class method)

=item validate(DATA[, NAME])

validate the given data using the named schema (or the default schema
if the name is not given)

=item options([NAME])

convert the named schema (or the default schema if the name is not
given) to a list of L<Getopt::Long> compatible options

=back

=head1 FUNCTIONS

The following convenient functions are available:

=over

=item is_regexp(SCALAR)

check if the given scalar is a compiled regular expression

=item string2hash(STRING)

convert a string of space separated key=value pairs into a hash
or hash reference

=item hash2string(HASH)

convert a hash or hash reference into a string of space separated
key=value pairs

=back

=head1 SCHEMAS

A schema is simply a structure (i.e. a hash reference) with the
following fields (all of them being optional except the first one):

=over

=item type

the type of the thing to validate (see the L</"TYPES"> section for the
complete list); this can also be a list of possible types
(e.g. C<integer> or C<undef>)

=item subtype

for an homogeneous list or table, the schema of its elements

=item fields

for a structure, a table of the allowed fields, in the form:
field name =E<gt> corresponding schema

=item optional

for a structure field, it indicates that the field is optional

=item min

the minimum length/size, only for some types
(integer, number, string, list and table)

=item max

the maximum length/size, only for some types
(integer, number, string, list and table)

=item match

a regular expression used to validate a string or table keys

=item check

a code reference allowing to run user-supplied code to further
validate the data

=back

As an example, the following schema describe what a valid schema is:

  {
    type   => "struct",
    fields => {
      type     => { type => "list?(valid(type))" },
      subtype  => { type => "valid(schema)",        optional => "true" },
      fields   => { type => "table(valid(schema))", optional => "true" },
      optional => { type => "boolean",              optional => "true" },
      min      => { type => "number",               optional => "true" },
      max      => { type => "number",               optional => "true" },
      match    => { type => "regexp",               optional => "true" },
      check    => { type => "code",                 optional => "true" },
    },
  }

=head1 NAMED SCHEMAS

For convenience and self-reference, schemas can be named.

To use named schemas, give them along with their names to the new() method:

  $validator = Config::Validator->new(
      name1 => { ... schema1 ... },
      name2 => { ... schema2 ... },
  );

You can then refer to them in the validate() method:

  $validator->validate($data, "name1");

If you don't need named schemas, you can use the simpler form:

  $validator = Config::Validator->new({ ... schema ... });
  $validator->validate($data);

=head1 TYPES

Here are the different types that can be used:

=over

=item anything

really anything, including undef

=item undef

the undefined value

=item undefined

synonym for C<undef>

=item defined

anything but undef

=item string

any string (in fact, anything that is defined and not a reference)

=item boolean

either C<true> or C<false>

=item number

any number (this is tested using a regular expression)

=item integer

any integer (this is tested using a regular expression)

=item reference

any reference, blessed or not

=item ref(*)

synonym for C<reference>

=item blessed

any blessed reference

=item object

synonym for C<blessed>

=item isa(*)

synonym for C<blessed>

=item unblessed

any reference which is not blessed

=item code

a code reference

=item regexp

a compiled regular expression

=item list

an homogeneous list

=item list(X)

idem but with the given subtype

=item list?(X)

shortcut for either C<X> or C<list(X)>

=item table

an homogeneous table

=item table(X)

idem but with the given subtype

=item struct

a structure, i.e. a table with known keys

=item ref(X)

a reference of the given kind

=item isa(X)

an object of the given kind

=item valid(X)

something valid according to the given named schema

=back

=head1 EXAMPLES

=head2 CONFIGURATION VALIDATION

This module works well with L<Config::General>. In particular, the
C<list?(X)> type matches the way L<Config::General> merges blocks.

For instance, one could use the following code:

  use Config::General qw(ParseConfig);
  use Config::Validator;
  $validator = Config::Validator->new(
    service => {
      type   => "struct",
      fields => {
        port  => { type => "integer", min => 0, max => 65535 },
        proto => { type => "string" },
      },
    },
    host => {
      type   => "struct",
      fields => {
        name    => { type => "string", match => qr/^\w+$/ },
        service => { type => "list?(valid(service))" },
      },
    },
  );
  %cfg = ParseConfig(-ConfigFile => $path, -CComments => 0);
  $validator->validate($cfg{host}, "host");

This would work with:

  <host>
    name = foo
    <service>
      port = 80
      proto = http
    </service>
  </host>

where C<$cfg{host}{service}> is the service hash but also with:

  <host>
    name = foo
    <service>
      port = 80
      proto = http
    </service>
    <service>
      port = 443
      proto = https
    </service>
  </host>

where C<$cfg{host}{service}> is the list of service hashes.

=head2 OPTIONS VALIDATION

This module interacts nicely with L<Getopt::Long>: the options()
method can be used to convert a schema into a list of L<Getopt::Long>
options.

Here is a simple example:

  use Config::Validator;
  use Getopt::Long qw(GetOptions);
  use Pod::Usage qw(pod2usage);
  $validator = Config::Validator->new({
    type   => "struct",
    fields => {
      debug => {
        type     => "boolean",
        optional => "true",
      },
      proto => {
        type  => "string",
        match => qr/^\w+$/,
      },
      port => {
        type => "integer",
        min  => 0,
        max  => 65535,
      },
    },
  });
  @options = $validator->options();
  GetOptions(\%cfg, @options) or pod2usage(2);
  $validator->validate(\%cfg);

=head1 AUTHOR

Lionel Cons L<http://cern.ch/lionel.cons>

Copyright CERN 2012
