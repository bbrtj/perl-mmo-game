package Game::Lore;

use My::Moose -strict;

use header;

has param 'children' => (
	isa => ArrayRef [InstanceOf ['Game::Lore']],
	default => sub { [] },
);

has option 'parent' => (
	isa => InstanceOf ['Game::Lore'],
	weak_ref => 1,
);

has param 'id' => (
	isa => Str,
);

has param 'name' => (
	isa => Str,
);

has param 'translations' => (
	isa => HashRef [Dict [name => Str, desc => Optional [Str]]],
	default => sub { {} },
);

has param 'uses' => (
	isa => ArrayRef [InstanceOf ['Game::Lore']],
	default => sub { [] },
);

has cached 'attributes' => (
	isa => ArrayRef,
	lazy => 1,
);

around BUILDARGS => sub ($orig, $self, %args) {
	$args{id} = join '.', $self->prefix, $args{id}
		if $args{id};
	return $self->$orig(%args);
};

sub BUILD ($self, @)
{
	state $repo = DI->get('lore_data_repo');
	$repo->save($self);

	if ($self->has_parent) {
		push $self->parent->children->@*, $self;
	}

	return;
}

sub _build_attributes ($self)
{
	return [grep { $_ isa 'Game::Lore::AttributeData' } $self->uses->@*];
}

sub prefix ($self)
{
	die 'Game::Lore has no prefix - needs a subclass';
}

