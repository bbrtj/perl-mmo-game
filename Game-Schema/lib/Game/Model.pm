package Game::Model;

use Mojo::Base -signatures;
use Moo::Role;
use Game::Types qw(Uuid);
use Game::Model::Role::Dummy;
use Carp;
use Scalar::Util qw(blessed);

my %orm_mapping;
my %orm_mapping_reverse;

has 'uuid' => (
	is => 'ro',
	isa => Uuid,
	coerce => 1,
	default => sub { undef },
);

sub _register ($class)
{
	if ($class =~ /Game::Model::(.+)/) {
		my $resultset = "Game::Schema::Result::$1";
		$orm_mapping{$class} = $resultset;
		$orm_mapping_reverse{$resultset} = $class;

		my @attributes = grep { $_->name !~ /^_/ } $class->meta->get_all_attributes;
		foreach my $attribute (@attributes) {
			my $name = $attribute->name;
			my $writer_attr = $attribute->meta->find_attribute_by_name("writer");

			if (!$writer_attr->get_value($attribute)) {
				$writer_attr->set_value($attribute, "set_$name");
			}
		}

		return $class->meta->make_immutable;
	}

	croak "cannot register $class";
}

sub serialize ($self)
{
	return {map { $_->name => $_->get_value($self) } grep { $_->has_value($self) } $self->meta->get_attribute_list};
}

sub from_result ($class, $row)
{
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

sub dummy ($class)
{
	croak "dummy only works on class context" if ref $class;
	return Game::Model::Role::Dummy->_make_dummy($class);
}

1;
