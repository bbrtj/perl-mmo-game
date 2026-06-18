package Model::Character;

use My::Moose;

use header;

extends 'Model';

with 'Model::Role::Stored';

has param 'player_id' => (
	isa => Maybe [ULID],
	default => undef,
	traits => [qw(Stored)],
);

has param 'class_id' => (
	isa => LoreId,
	traits => [qw(Stored)],
);

has param 'race_id' => (
	isa => LoreId,
	traits => [qw(Stored)],
);

has param 'alliance_id' => (
	isa => LoreId,
	traits => [qw(Stored)],
);

has param 'name' => (
	isa => ShortStr,
	traits => [qw(Stored)],
);

has cached 'class' => (
	lax_isa => InstanceOf ['Game::Lore::Class'],
	lazy => 1,
);

has cached 'race' => (
	lax_isa => InstanceOf ['Game::Lore::Race'],
	lazy => 1,
);

has cached 'alliance' => (
	lax_isa => InstanceOf ['Game::Lore::Alliance'],
	lazy => 1,
);

DI->static_injected('lore_data_repo');

sub is_player ($self)
{
	return defined $self->player_id;
}

sub _build_class ($self)
{
	return $self->lore_data_repo->load($self->class_id);
}

sub _build_race ($self)
{
	return $self->lore_data_repo->load($self->race_id);
}

sub _build_alliance ($self)
{
	return $self->lore_data_repo->load($self->alliance_id);
}

__PACKAGE__->_register;

