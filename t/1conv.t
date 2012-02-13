#!perl

use strict;
use warnings;
use Config::Validator qw(string2hash hash2string);
use Test::More tests => 11;

our(%hash);

is(hash2string(), "");
is(hash2string({}), "");
is(hash2string({ abc => 123 }), "abc=123");
is(hash2string({ abc => 123, def => "", ghi => "def" }), "abc=123 def= ghi=def");
is(hash2string({ abc => "<%>" }), "abc=%3C%25%3E");

%hash = string2hash("");
is(keys(%hash), 0);

%hash = string2hash("abc=123");
is(keys(%hash), 1);
is((keys(%hash))[0], "abc");

%hash = string2hash("%3C%25%3E=123 456=");
is(keys(%hash), 2);
is(join("|", sort(keys(%hash))), "456|<%>");
is(join("|", sort(values(%hash))), "|123");
