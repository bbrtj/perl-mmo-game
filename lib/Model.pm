package Model;

use My::Moose -traits => [qw(
	My::Moose::Trait::AutoSetters
	My::Moose::Trait::Serializable
	My::Moose::Trait::FakeRequired
)];

use Carp;

use header;

my %orm_mapping;
my %orm_mapping_reverse;

my %cache_mapping;
my %cache_mapping_reverse;

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

sub _register_cache ($class)
{
	if ($class =~ /Model::(.+)/) {
		$cache_mapping{$class} = $1;
		$cache_mapping_reverse{$1} = $class;

		return;
	}

	croak "cannot register $class";
}

sub from_result ($class, $row)
{
	my $resultset = blessed $row;
	croak "invalid argument to from_result"
		unless defined $resultset && $orm_mapping_reverse{$resultset};

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

sub from_cache ($class, $name, $hash)
{
	croak "invalid argument to from_cache"
		unless defined $name && $cache_mapping_reverse{$name};

	my $real_class = $cache_mapping_reverse{$name};
	return $real_class->new($hash);
}

sub get_result_class ($self)
{
	my $class = blessed $self // $self;

	croak 'invalid argument for get_result_class'
		unless $orm_mapping{$class};

	return $orm_mapping{$class};
}

sub get_cache_name ($self)
{
	my $class = blessed $self // $self;

	croak 'invalid argument for get_cache_name'
		unless $cache_mapping{$class};

	return $cache_mapping{$class};
}

