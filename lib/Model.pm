package Model;

use My::Moose -traits => [
	qw(
		My::Moose::Trait::AutoSetters
		My::Moose::Trait::Serializable
		My::Moose::Trait::FakeRequired
	)
];

use Carp;
use Mojo::Loader qw(load_classes);
use Schema;

use header;

my %orm_mapping;
my %orm_mapping_reverse;

my %cache_mapping;
my %cache_mapping_reverse;

load_classes('Model');

sub _register ($class)
{
	croak "$class needs to mix Model::Role::Stored"
		unless $class->DOES('Model::Role::Stored');

	croak "cannot register $class"
		unless $class =~ /Model::(.+)/;

	my $resultset = "Schema::Result::$1";
	$orm_mapping{$class} = $resultset;
	$orm_mapping_reverse{$resultset} = $class;

	$resultset->add_columns(map { $_->name } $class->meta->serialized_attributes->@*);
	Schema->register_class($1, $resultset);

	return;
}

sub _register_cache ($class)
{
	croak "$class needs to mix Model::Role::Identified"
		unless $class->DOES('Model::Role::Identified');

	croak "cannot register $class"
		unless $class =~ /Model::(.+)/;

	$cache_mapping{$class} = $1;
	$cache_mapping_reverse{$1} = $class;
	return;
}

sub from_result ($class, $row)
{
	my $resultset = blessed $row;
	croak "invalid argument to from_result"
		unless defined $resultset && $orm_mapping_reverse{$resultset};

	# NOTE: in case of performance problems, this can be just a bless
	my $real_class = $orm_mapping_reverse{$resultset};
	return $real_class->new($row->get_columns);
}

sub from_cache ($class, $name, $hash)
{
	croak "invalid argument to from_cache"
		unless defined $name && $cache_mapping_reverse{$name};

	# NOTE: in case of performance problems, this can be just a bless
	my $real_class = $cache_mapping_reverse{$name};
	return $real_class->new($hash);
}

sub get_result_class ($self)
{
	my $class = blessed $self // $self;

	croak 'invalid argument for get_result_class'
		unless exists $orm_mapping{$class};

	return $orm_mapping{$class};
}

sub get_cache_name ($self)
{
	my $class = blessed $self // $self;

	croak 'invalid argument for get_cache_name'
		unless exists $cache_mapping{$class};

	return $cache_mapping{$class};
}

