package Game::LoreData;

use My::Moose;

use header;

has param 'main_obj' => (
	isa => InstanceOf ['Game::Lore'],
	weak_ref => 1,
);

has param 'translations' => (
	isa => HashRef,
	default => sub { {} },
);

has param 'define' => (
	isa => HashRef,
	default => sub { {} },
);

has param 'uses' => (
	isa => ArrayRef,
	default => sub { [] },
);

has param 'children' => (
	isa => ArrayRef [InstanceOf ['Game::Lore']],
	default => sub { [] },
);

has option 'parent' => (
	writer => -hidden,
	isa => InstanceOf ['Game::Lore'],
	weak_ref => 1,
);

has cached 'attributes' => (
	isa => ArrayRef,
	lazy => 1,
);

sub _build_attributes ($self)
{
	return [grep { $_ isa 'Game::Lore::AttributeData' } $self->uses->@*];
}

# little hack to allow reversal of parent-children defining

sub set_parent ($self, $parent)
{
	$self->_set_parent($parent);
	push $parent->data->children->@*, $self->main_obj;
	return;
}

