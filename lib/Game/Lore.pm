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

has option 'visuals' => (
	isa => Dict [
		model => Optional [Str],
		model_size => Optional [PositiveNum],
	],
);

has param 'uses' => (
	isa => ArrayRef [InstanceOf ['Game::Lore']],
	default => sub { [] },
);

has cached 'primary_stats' => (
	isa => ArrayRef,
	lazy => 1,
);

has cached 'secondary_stats' => (
	isa => ArrayRef,
	lazy => 1,
);

has cached 'attributes' => (
	isa => ArrayRef,
	lazy => 1,
);

has cached 'classes' => (
	isa => ArrayRef,
	lazy => 1,
);

has cached 'races' => (
	isa => ArrayRef,
	lazy => 1,
);

has cached 'abilities' => (
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

sub _build_primary_stats ($self)
{
	return [grep { $_ isa 'Game::Lore::PrimaryStat' } $self->uses->@*];
}

sub _build_secondary_stats ($self)
{
	return [grep { $_ isa 'Game::Lore::SecondaryStat' } $self->uses->@*];
}

sub _build_attributes ($self)
{
	return [grep { $_ isa 'Game::Lore::AttributeData' } $self->uses->@*];
}

sub _build_classes ($self)
{
	return [grep { $_ isa 'Game::Lore::Class' } $self->uses->@*];
}

sub _build_races ($self)
{
	return [grep { $_ isa 'Game::Lore::Race' } $self->uses->@*];
}

sub _build_abilities ($self)
{
	return [grep { $_ isa 'Game::Lore::Ability' } $self->uses->@*];
}

sub prefix ($self)
{
	die 'Game::Lore has no prefix - needs a subclass';
}

