package Game::Lore;

use My::Moose;
use Types;

use header;

has 'id' => (
	is => 'ro',
	required => 1,
	isa => Types::Str,
);

has 'name' => (
	is => 'ro',
	required => 1,
	isa => Types::Str,
);

my %collection;
my %named_collection;

around BUILDARGS => sub ($orig, $self, %args) {
	$args{id} = join '.', 'L', $self->prefix, $args{id}
		if $args{id};
	return $self->$orig(%args);
};

sub BUILD ($self, @)
{
	$named_collection{blessed $self}{$self->name} = $self;
	return;
}

sub get_named ($self, $class, $name)
{
	return $named_collection{$class}{$name};
}

sub get_all_named ($self)
{
	return \%named_collection;
}

sub data ($self)
{
	my $id = $self->id;

	if (!exists $collection{$id}) {
		my $target_class = blessed($self) . 'Data';
		$collection{$id} = $target_class->new;
	}

	return $collection{$id};
}

sub prefix ($self)
{
	die 'Game::Lore has no prefix - needs a subclass';
}

