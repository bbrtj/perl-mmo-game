package Game::LoreData;

use My::Moose;

use header;

has 'main_obj' => (
	is => 'ro',
	isa => Types::InstanceOf ['Game::Lore'],
	weak_ref => 1,
);

has 'translations' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub { {} },
);

has 'define' => (
	is => 'ro',
	isa => Types::HashRef,
	default => sub { {} },
);

has 'uses' => (
	is => 'ro',
	isa => Types::ArrayRef,
	default => sub { [] },
);

has 'children' => (
	is => 'ro',
	isa => Types::ArrayRef [Types::InstanceOf ['Game::Lore']],
	default => sub { [] },
);

has 'parent' => (
	is => 'ro',
	writer => '_set_parent',
	isa => Types::InstanceOf ['Game::Lore'],
	weak_ref => 1,
);

# little hack to allow reversal of parent-children defining

sub set_parent ($self, $parent)
{
	$self->_set_parent($parent);
	push $parent->data->children->@*, $self->main_obj;
	return;
}

