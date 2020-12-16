package Game::Model;

use Mojo::Base -signatures;
use Moo::Role;
use Game::Types qw(Uuid);
use Carp;
use Scalar::Util qw(blessed);

my %orm_mapping;
my %orm_mapping_reverse;

has 'uuid' => (
	is => 'ro',
	isa => Uuid,
	default => sub { Uuid->generate },
);

sub _register($class)
{
	if ($class =~ /Game::Model::(.+)/) {
		my $resultset = "Game::Schema::Result::$1";
		$orm_mapping{$class} = $resultset;
		$orm_mapping_reverse{$resultset} = $class;
		return $class->meta->make_immutable;
	}

	croak "cannot register $class";
}

sub get_data($self)
{
	my $meta = $self->meta;

	return map {
		$_ => $meta->get_attribute($_)->get_value($self)
	} $meta->get_attribute_list;
}

sub from_result($class, $row) {
	my $resultset = blessed $row;
	croak "invalid argument to from_result"
		unless defined $resultset;

	my $real_class = $orm_mapping_reverse{$resultset};
	return $real_class->new(
		map {
			my $sub = $row->can($_);
			croak "cannot fetch $_ from $resultset result set"
				unless $sub;

			$_ => $sub->($row);
		} $real_class->meta->get_attribute_list
	);
}

1;
