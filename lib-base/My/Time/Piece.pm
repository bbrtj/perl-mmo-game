package My::Time::Piece;

use Time::Piece;

use v5.42;

# lightweight object, blessed reference to Time::Piece for minimal effort

use overload
	'""' => sub { shift->to_string },
	'0+' => sub { refaddr shift },
	fallback => true,
	;

my $format = '%Y-%m-%d %H:%M:%S%z';
my %instances;

sub new ($class, $obj)
{
	my $var = undef;
	my $instance = bless \$var, $class;
	$instances{0 + $instance} = $obj;

	# this trick allows us to have a nicer representation of time in dumps
	# (which also compares better in tests)
	$var = $obj->epoch;

	return $instance;
}

sub DESTROY ($self)
{
	delete $instances{0 + $self};
}

sub obj ($self)
{
	return $instances{0 + $self};
}

sub from_timestamp ($class, $timestamp)
{
	return $class->new(Time::Piece->new($timestamp));
}

sub from_string ($class, $string)
{
	$string =~ s{(\+\d\d)$}{${1}00};
	return $class->new(Time::Piece->strptime($string, $format));
}

sub to_string ($self)
{
	my $string = $self->obj->strftime($format);

	# remove minute offset to match pg format
	return substr $string, 0, -2;
}

