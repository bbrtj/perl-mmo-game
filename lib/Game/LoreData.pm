package Game::LoreData;

use My::Moose;

use header;

has param 'main_obj' => (
	isa => Types::InstanceOf ['Game::Lore'],
	weak_ref => 1,
);

has param 'translations' => (
	isa => Types::HashRef,
	default => sub { {} },
);

has param 'define' => (
	isa => Types::HashRef,
	default => sub { {} },
);

has param 'uses' => (
	isa => Types::ArrayRef,
	default => sub { [] },
);

has param 'children' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Game::Lore']],
	default => sub { [] },
);

has option 'parent' => (
	writer => -hidden,
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

