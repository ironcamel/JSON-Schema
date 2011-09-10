use strict;
use warnings;

use inc::Module::Install;

my $dist = 'JSON-Schema';
my $fn   = "lib/$dist.pm"; $fn =~ s#-#/#g;

name                $dist;
perl_version_from   $fn;
version_from        $fn;
abstract_from       $fn;
readme_from         $fn;
author              'Toby Inkster <tobyink@cpan.org>';
license             'perl';

requires            'Carp'               => '1.00';
requires            'common::sense'      => 0;
requires            'HTTP::Link::Parser' => '0.100';
requires            'JSON'               => '2.00';
requires            'JSON::Hyper'        => 0;
requires            'JSON::Path'         => 0;
requires            'LWP::UserAgent'     => 0;
requires            'Scalar::Util'       => 0;
test_requires       'Test::More'         => '0.61';

keywords 'JSON', 'schema', 'validate';

resources(
	'homepage'   => "http://search.cpan.org/dist/$dist/",
	'repository' => "http://goddamn.co.uk/viewvc/perlmods/$dist/",
	'bugtracker' => "http://rt.cpan.org/Dist/Display.html?Queue=$dist",
	);
	
write_doap_changes;
write_doap_changes_xml;

include 'Test::Signature';
auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);
