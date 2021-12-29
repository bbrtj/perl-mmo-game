package Model;

use My::Moose -traits => [qw(AutoSetters Serializable)];
use Model::Role::Dummy;
use Carp;
use Scalar::Util qw(blessed);

use header;

my %orm_mapping;
my %orm_mapping_reverse;

sub _register ($class)
{
	if ($class =~ /Model::(.+)/) {
		my $resultset = "Schema::Result::$1";
		$orm_mapping{$class} = $resultset;
		$orm_mapping_reverse{$resultset} = $class;

		return;
	}

	croak "cannot register $class";
}

sub from_result ($class, $row)
{
	my $resultset = blessed $row;
	croak "invalid argument to from_result"
		unless defined $resultset;

	my $real_class = $orm_mapping_reverse{$resultset};
	return $real_class->new(
		map {
			my $name = $_->name;
			my $sub = $row->can($name);
			croak "cannot fetch $name from $resultset result set"
				unless $sub;

			$name => $sub->($row);
		} $real_class->meta->serialized_attributes->@*
	);
}

sub get_result_class ($self)
{
	my $class = blessed $self // $self;

	croak 'invalid argument for get_result_class'
		unless exists $orm_mapping{$class};

	return $orm_mapping{$class};
}

sub dummy ($class)
{
	croak "dummy only works on class context" if ref $class;
	return Model::Role::Dummy->_make_dummy($class);
}

