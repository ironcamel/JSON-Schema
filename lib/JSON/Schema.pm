package JSON::Schema;

use 5.008;
use common::sense;

use Carp;
use HTTP::Link::Parser qw[parse_links_to_rdfjson relationship_uri];
use JSON;
use JSON::Hyper;
use JSON::Schema::Error;
use JSON::Schema::Result;
use LWP::UserAgent;

our $VERSION = '0.001_01';

sub new
{
	my ($class, $schema) = @_;
	$schema = from_json($schema) unless ref $schema;
	return bless {schema=>$schema}, $class;
}

sub detect
{
	my ($class, $source) = @_;
	
	my $hyper = JSON::Hyper->new;
	my ($object, $url);
	
	if ($source->isa('HTTP::Response'))
	{
		$url    = $source->request->uri;
		$object = from_json($source->decoded_content);
	}
	else
	{
		$url = "$source";
		($source, my $frag) = split /\#/, $source, 2;
		($object, $source) = $hyper->_get($source);
		$object = fragment_resolve($object, $frag);
	}
	
	# Link: <>; rel="describedby"
	my $links  = parse_links_to_rdfjson($source);
	my @schema =
		map { $class->new( $hyper->get($_->{'value'}) ) }
		grep { lc $_->{'type'} eq 'uri' }
		$links->{$url}->{relationship_uri('describedby')};
	
	# ;profile=
	push @schema,
		map { $class->new( $hyper->get($_) ) }
		map { if (/^\'/) { s/(^\')|(\'$)//g } elsif (/^\"/) { s/(^\")|(\"$)//g } else { $_ } }
		map { s/^profile=// }
		grep /^profile=/,	$source->content_type;
	
	# $schema links
	if ($object)
	{
		push @schema,
			map { $class->new( $hyper->get($_->{'href'}) ) }
			grep { lc $_->{'rel'} eq 'describedby' or lc $_->{'rel'} eq relationship_uri('describedby') }
			$hyper->find_links($object);
	}
	return @schema;
}

sub schema
{
	my ($self) = @_;
	return $self->{'schema'};
}

sub validate
{
	my ($self, $object) = @_;
	$object = from_json($object) unless ref $object;
	
	my $helper = JSON::Schema::Helper->new;
	my $result = $helper->validate($object, $self->schema);
	return JSON::Schema::Result->new($result);
}

sub ua
{
	my $self = shift;	
	$self = {} unless blessed($self);
	
	if (@_)
	{
		my $rv = $self->{'ua'};
		$self->{'ua'} = shift;
		croak "Set UA to something that is not an LWP::UserAgent!"
			unless blessed $self->{'ua'} && $self->{'ua'}->isa('LWP::UserAgent');
		return $rv;
	}
	unless (blessed $self->{'ua'} && $self->{'ua'}->isa('LWP::UserAgent'))
	{
		$self->{'ua'} = LWP::UserAgent->new(agent=>sprintf('%s/%s ', __PACKAGE__, $VERSION));
		$self->{'ua'}->default_header('Accept'=>'application/json, application/schema+json');
	}
	return $self->{'ua'};
}

1;

package JSON::Schema::Helper;

### 
 # JSONSchema Validator - Validates JavaScript objects using JSON Schemas 
 #	(http://www.json.com/json-schema-proposal/)
 #
 # Copyright (c) 2007 Kris Zyp SitePen (www.sitepen.com)
 # Licensed under the MIT (MIT-LICENSE.txt) license.
#To use the validator call JSONSchema.validate with an instance object and an optional schema object.
#If a schema is provided, it will be used to validate. If the instance object refers to a schema (self-validating), 
#that schema will be used to validate and the schema parameter is not necessary (if both exist, 
#both validations will occur). 
#The validate method will return an array of validation errors. If there are no errors, then an 
#empty list will be returned. A validation error will have two properties: 
#"property" which indicates which property had the error
#"message" which indicates what the error was
 ##

use 5.008;
use common::sense;
use constant FALSE => 0;
use constant TRUE  => 1;

use JSON::Hyper;
use POSIX qw[modf];
use Scalar::Util qw[blessed];

our $VERSION = '0.001_01';

sub new
{
	my ($class, %args) = @_;
	$args{'hyper'}  ||= JSON::Hyper->new;
	$args{'errors'} ||= [];
	return bless \%args, $class;
}

sub validate
{
	my ($self, $instance, $schema) = @_;
	## Summary:
	##  	To use the validator call JSONSchema.validate with an instance object and an optional schema object.
	## 		If a schema is provided, it will be used to validate. If the instance object refers to a schema (self-validating), 
	## 		that schema will be used to validate and the schema parameter is not necessary (if both exist, 
	## 		both validations will occur). 
	## 		The validate method will return an object with two properties:
	## 			valid: A boolean indicating if the instance is valid by the schema
	## 			errors: An array of validation errors. If there are no errors, then an 
	## 					empty list will be returned. A validation error will have two properties: 
	## 						property: which indicates which property had the error
	## 						message: which indicates what the error was
	##
	return $self->_validate($instance, $schema, FALSE);
}

sub checkPropertyChange
{
	my ($self, $value, $schema, $property) = @_;
	## Summary:
	## 		The checkPropertyChange method will check to see if an value can legally be in property with the given schema
	## 		This is slightly different than the validate method in that it will fail if the schema is readonly and it will
	## 		not check for self-validation, it is assumed that the passed in value is already internally valid.  
	## 		The checkPropertyChange method will return the same object type as validate, see JSONSchema.validate for 
	## 		information.
	##
	return $self->_validate($value, $schema, $property||'property');	
}

sub _validate
{
	my ($self, $instance, $schema, $_changing) = @_;
	
	$self->{errors} = [];
	
	if ($schema)
	{
		$self->checkProp($instance, $schema, '', $_changing || '', $_changing);
	}
	if(!$_changing and defined $instance and ref $instance eq 'HASH' and defined $instance->{'$schema'})
	{
		$self->checkProp($instance, $instance->{'$schema'}, '', '', $_changing);
	}
	
	return { valid=>(@{$self->{errors}} ? FALSE : TRUE), errors=> $self->{errors} };
}

sub checkType
{
	my ($self, $type, $value, $path, $_changing, $schema) = @_;
	
	my @E;
	my $addError = sub
	{
		my ($message) = @_;
		my $e = { property=>$path, message=>$message };
		foreach (qw(title description))
		{
			$e->{$_} = $schema->{$_}
				if defined $schema->{$_};
		}
		push @E, $e;
	};
	
	if ($type)
	{
#		if (ref $type ne 'HASH'
#		and $type ne 'any'
#		and ($type eq 'null' ? $self->jsIsNull($value) : $self->jsMatchType($type, $value))
#		and !(ref $value eq 'ARRAY' and $type eq 'array')
#		and !($type eq 'integer' and $value % 1 == 0))
		if (!$self->jsMatchType($type, $value))
		{
			$addError->($self->jsGuessType($value)." value found, but a $type is required");
			return @E;
		}
		if (ref $type eq 'ARRAY')
		{
			my @unionErrors;
			TYPE: foreach my $t (@$type)
			{
				@unionErrors = @{ $self->checkType($t, $value, $path, $_changing, $schema) };
				last unless @unionErrors;
			}
			return @unionErrors if @unionErrors;
		}
		elsif (ref $type eq 'HASH')
		{
			local $self->{errors} = [];
			checkProp($value, $type, $path, undef, $_changing);
			return @{ $self->{errors} };
		}
	}
	return;
}

# validate a value against a property definition
sub checkProp
{
	my ($self, $value, $schema, $path, $i, $_changing) = @_;
	my $l;
	$path .= $path ? ".${i}" : "\$${i}";
	
	my $addError = sub
	{
		my ($message) = @_;
		my $e = { property=>$path, message=>$message };
		foreach (qw(title description))
		{
			$e->{$_} = $schema->{$_}
				if defined $schema->{$_};
		}
		push @{$self->{errors}}, $e;
	};
	
	if (ref $schema ne 'HASH' and ($path or ref $schema ne 'CODE'))
	{
		if (ref $schema eq 'CODE')
		{
			# ~TOBYINK: I don't think this makes any sense in Perl
			$addError->("is not an instance of the class/constructor " . $schema);
		}
		elsif ($schema)
		{
			$addError->("Invalid schema/property definition " . $schema);
		}
		return undef;
	}
	
	$self->{'hyper'}->process_includes($schema, $self->{'base'});
	
	if ($_changing and $schema->{'readonly'})
	{
		$addError->("is a readonly field, it can not be changed");
	}
	if ($schema->{'extends'})
	{
		checkProp($value, $schema->{'extends'}, $path, $i, $_changing);
	}
	
	# validate a value against a type definition
	if (!defined $value)
	{
		$addError->("is missing and it is not optional")
			unless $schema->{'optional'};
	}
	else
	{
		push @{$self->{errors}}, $self->checkType($schema->{'type'}, $value, $path, $_changing, $schema);
		if (defined $schema->{'disallow'}
		and !$self->checkType($schema->{'disallow'}, $value, $path, $_changing))
		{
			$addError->(" disallowed value was matched");
		}
		if (!$self->jsIsNull($value))
		{
			if (ref $value eq 'ARRAY')
			{
				if (ref $schema->{'items'} eq 'ARRAY')
				{
					for (my $i=0; $i < scalar @{ $schema->{'items'} }; $i++)
					{
						my $x = defined $value->[$i] ? $value->[$i] : JSON::Schema::Null->new; 
						push @{$self->{errors}}, checkProp($x, $schema->{'items'}->[$i], $path, $i, $_changing);
					}
				}
				elsif (defined $schema->{'items'})
				{
					for (my $i=0; $i < scalar @{ $schema->{'items'} }; $i++)
					{
						my $x = defined $value->[$i] ? $value->[$i] : JSON::Schema::Null->new; 
						push @{$self->{errors}}, checkProp($x, $schema->{'items'}, $path, $i, $_changing);
					}
				}
				if ($schema->{'minItems'}
				and scalar @$value < $schema->{'minItems'})
				{
					addError->("There must be a minimum of " . $schema->{'minItems'} . " in the array");
				}
				if ($schema->{'maxItems'}
				and scalar @$value > $schema->{'maxItems'})
				{
					addError->("There must be a maximum of " . $schema->{'maxItems'} . " in the array");
				}
			}
			elsif ($schema->{'properties'})
			{
				push @{$self->{errors}}, $self->checkObj($value, $schema->{'properties'}, $path, $schema->{'additionalProperties'}, $_changing);
			}
			if ($schema->{'pattern'} and $self->jsMatchType('string', $value))
			{
				my $x = $schema->{'pattern'};
				$addError->("does not match the regex pattern $x")
					unless $value =~ /$x/;
			}
			if ($schema->{'maxLength'} and $self->jsMatchType('string', $value)
			and strlen($value) > $schema->{'maxLength'})
			{
				$addError->("may only be " . $schema->{'maxLength'} . " characters long");
			}
			if ($schema->{'minLength'} and $self->jsMatchType('string', $value)
			and strlen($value) < $schema->{'minLength'})
			{
				$addError->("must be at least " . $schema->{'minLength'} . " characters long");
			}
			if (defined $schema->{'minimum'} and not $self->jsMatchType('number', $value))
			{
				if (defined $schema->{'minimumCanEqual'} and not $schema->{'minimumCanEqual'})
				{
					$addError->("must be greater than minimum value '" . $schema->{'minimum'}) . "'"
						if $value lt $schema->{'minimum'};
				}
				else
				{
					$addError->("must be greater than or equal to minimum value '" . $schema->{'minimum'}) . "'"
						if $value le $schema->{'minimum'};
				}
			}
			elsif (defined $schema->{'minimum'})
			{
				if (defined $schema->{'minimumCanEqual'} and not $schema->{'minimumCanEqual'})
				{
					$addError->("must be greater than minimum value " . $schema->{'minimum'})
						unless $value > $schema->{'minimum'};
				}
				else
				{
					$addError->("must be greater than or equal to minimum value " . $schema->{'minimum'})
						unless $value >= $schema->{'minimum'};
				}
			}
			if (defined $schema->{'maximum'} and not $self->jsMatchType('number', $value))
			{
				if (defined $schema->{'maximumCanEqual'} and not $schema->{'maximumCanEqual'})
				{
					$addError->("must be less than or equal to maximum value '" . $schema->{'maximum'}) . "'"
						if $value gt $schema->{'maximum'};
				}
				else
				{
					$addError->("must be less than or equal to maximum value '" . $schema->{'maximum'}) . "'"
						if $value ge $schema->{'maximum'};
				}
			}
			elsif (defined $schema->{'maximum'})
			{
				if (defined $schema->{'maximumCanEqual'} and not $schema->{'maximumCanEqual'})
				{
					$addError->("must be less than maximum value " . $schema->{'maximum'})
						unless $value < $schema->{'maximum'};
				}
				else
				{
					$addError->("must be less than or equal to maximum value " . $schema->{'maximum'})
						unless $value <= $schema->{'minimum'};
				}
			}
			if ($schema->{'enum'})
			{
				$addError->("does not have a value in the enumeration {" . (join ",", @{ $schema->{'enum'} }) . '}')
					unless grep { $value eq $_ } @{ $schema->{'enum'} };
			}
			if ($schema->{'divisibleBy'} and $self->jsMatchType('number', $value))
			{
				my ($frac,$int) = modf($value / $schema->{'divisibleBy'});
				$addError->("must be divisible by " . $schema->{'divisibleBy'})
					if $frac;
			}
			elsif ($schema->{'maxDecimal'} and $self->jsMatchType('number', $value)) # ~TOBYINK: back-compat
			{
				my $regexp = "\\.[0-9]{" . ($schema->{'maxDecimal'} + 1) . ",}";
				$addError->("may only have " . $schema->{'maxDecimal'} . " digits of decimal places")
					if $value =~ /$regexp/;
			}
		} # END: if (!$self->jsIsNull()) { ... }
	} # END: if (!$defined $value) {} else {...}
	return;
}; # END: sub checkProp


sub checkObj
{
	my ($self, $instance, $objTypeDef, $path, $additionalProp, $_changing) = @_;
	my @errors;
	
	my $addError = sub
	{
		my ($message) = @_;
		my $e = { property=>$path, message=>$message };
		push @{$self->{errors}}, $e;
	};
	
	$self->{'hyper'}->process_includes($objTypeDef, $self->{'base'});
	
	if (ref $objTypeDef eq 'HASH')
	{
		if (ref $instance ne 'HASH')
		{
			$addError->("an object is required");
		}
		
		foreach my $i (keys %$objTypeDef)
		{
			unless ($i =~ /^__/)
			{
				my $value   = defined $instance->{$i} ? $instance->{$i} : exists $instance->{$i} ? JSON::Schema::Null->new : undef;
				my $propDef = $objTypeDef->{$i};
				$self->checkProp($value, $propDef, $path, $i, $_changing);
			}
		}
	} # END: if (ref $objTypeDef eq 'HASH')
	foreach my $i (keys %$instance)
	{
		if ($i !~ /^__/
		and defined $objTypeDef
		and not defined $objTypeDef->{$i}
		and not defined $additionalProp)
		{
			$addError->("The property $i is not defined in the schema and the schema does not allow additional properties");
		}
		my $requires = $objTypeDef && $objTypeDef->{$i} && $objTypeDef->{$i}->{'requires'};
		if (defined $requires and not defined $instance->{$requires})
		{
			$addError->("the presence of the property $i requires that $requires also be present");
		}
		my $value = defined $instance->{$i} ? $instance->{$i} : exists $instance->{$i} ? JSON::Schema::Null->new : undef;
		if (defined $objTypeDef
		and ref $objTypeDef eq 'HASH'
		and !defined $objTypeDef->{$i})
		{
			$self->checkProp($value, $additionalProp, $path, $i, $_changing); 
		}
		if(!$_changing and defined $value and ref $value eq 'HASH' and defined $value->{'$schema'})
		{
			push @errors, $self->checkProp($value, $value->{'$schema'}, $path, $i, $_changing);
		}
	}
	return @errors;
}

sub jsIsNull
{
	my ($self, $value) = @_;
	
	return TRUE if blessed($value) && $value->isa('JSON::Schema::Null');
	
	return FALSE;
}

sub jsMatchType
{
	my ($self, $type, $value) = @_;
	
	if (lc $type eq 'string')
	{
		return (ref $value) ? FALSE : TRUE;
	}

	if (lc $type eq 'number')
	{
		return ($value =~ /^\-?[0-9]*(\.[0-9]*)?$/) ? TRUE : FALSE;
	}
	
	if (lc $type eq 'integer')
	{
		return ($value =~ /^\-?[0-9]+$/) ? TRUE : FALSE;
	}
	
	if (lc $type eq 'boolean')
	{
		return (ref $value eq 'SCALAR' and $$value==0 and $$value==1) ? TRUE : FALSE;
	}

	if (lc $type eq 'object')
	{
		return (ref $value eq 'HASH') ? TRUE : FALSE;
	}
	
	if (lc $type eq 'array')
	{
		return (ref $value eq 'ARRAY') ? TRUE : FALSE;
	}

	if (lc $type eq 'null')
	{
		return $self->jsIsNull($value);
	}
	
	if (lc $type eq 'any')
	{
		return TRUE;
	}
	
	if (lc $type eq 'none')
	{
		return FALSE;
	}
	
	if (blessed($value) and $value->isa($type))
	{
		return TRUE;
	}
	elsif (ref($value) and ref($value) eq $type)
	{
		return TRUE;
	}
	
	return FALSE;
}

sub jsGuessType
{
	my ($self, $value) = @_;
	
	return 'object'
		if ref $value eq 'HASH';

	return 'array'
		if ref $value eq 'ARRAY';

	return 'boolean'
		if (ref $value eq 'SCALAR' and $$value==0 and $$value==1);
	
	return 'null'
		if $self->jsIsNull($value);
		
	return ref $value
		if ref $value;

	return 'integer'
		if $value =~ /^\-?[0-9]+$/;
	
	return 'number'
		if $value =~ /^\-?[0-9]*(\.[0-9]*)?$/;
	
	return 'string';
}

1;

package JSON::Schema::Null;

use 5.008;
use common::sense;
use overload '""' => sub { return '' };

our $VERSION = '0.001_01';

sub new
{
	my ($class) = @_;
	my $x = '';
	return bless \$x, $class;
}

sub TO_JSON
{
	return undef;
}

1;

__END__

=head1 NAME

JSON::Schema - validate JSON against a schema

=head1 SYNOPSIS

 my $validator = JSON::Schema->new($schema);
 my $json      = from_json( ... );
 my $result    = $validator->validate($json);
 
 if ($result)
 {
   print "Valid!\n";
 }
 else
 {
   print "Errors\n";
	print " - $_\n" foreach $result->errors;
 }

=head1 DESCRIPTION

=head2 Constructors

=over 4

=item C<< JSON::Schema->new($schema) >>

Given a JSON (or equivalent Perl nested hashref/arrayref structure)
Schema, returns a Perl object capable of checking objects against
that schema.

Note that some schemas contain '$ref' properties which act as
inclusions; this module does not expand those, but the L<JSON::Hyper>
module can.

=item C<< JSON::Schema->detect($url) >>

Given the URL for a JSON instance (or an HTTP::Response object)
returns a list of schemas (as JSON::Schema objects) that the
JSON instance claims to conform to, detected from the HTTP
response headers.

=back

=head2 Methods

=over 4

=item C<< schema >>

Returns the original schema as a hashref/arrayref structure.

=item C<< validate($object) >>

Validates the object against the schema and returns a
L<JSON::Schema::Result>.

=back

=head2 Perl Specifics

Perl uses weak typing. This module largely gives JSON instances
the benefit of the doubt. For example, if something looks like a
number (e.g. a string which only includes the digits 0 to 9)
then it will validate against schemas that require a number.

The module extends JSON Schema's native set of types ('string',
'number', 'integer', 'boolean', 'array', 'object', 'null', 'any')
with any Perl package name. i.e. the following is valid:

  my $validator = JSON::Schema->new({
    properties => { 
      'time' => { type => ['DateTime','string'] },
	 },
  });
  my $object = {
    'time' => DateTime->now;
  };
  my $result = $schema->validate($object);

This extension makes JSON::Schema useful not just for validating
JSON structures, but acyclic Perl structures generally.

Acyclic. Yes, acyclic. You don't want an infinite loop.

=head1 BUGS

Please report any bugs to L<http://rt.cpan.org/>.

=head1 SEE ALSO

L<JSON::Schema::Result>, L<JSON::Schema::Error>, L<JSON::Schema::Examples>.

Related modules: L<JSON::T>, L<JSON::Path>, L<JSON::GRDDL>,
L<JSON::Hyper>.

L<http://tools.ietf.org/html/draft-zyp-json-schema>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

This is largely a port of Kris Zyp's Javascript JSON Schema validator
L<http://code.google.com/p/jsonschema/>.

=head1 COPYRIGHT AND LICENCE

Copyright 2007-2009 Kris Zyp.

Copyright 2010 Toby Inkster.

This module is tri-licensed. It is available under the X11 (a.k.a. MIT)
licence; you can also redistribute it and/or modify it under the same
terms as Perl itself.

=head2 a.k.a. "The MIT Licence"

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

=cut
