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

has param 'name' => (
	isa => ShortStr,
	traits => [qw(Stored)],
);

has field 'class' => (
	lax_isa => InstanceOf ['Game::Lore::Class'],
	lazy => 1,
);

has field 'race' => (
	lax_isa => InstanceOf ['Game::Lore::Race'],
	lazy => 1,
);

sub is_player ($self)
{
	return defined $self->player_id;
}

sub _build_class ($self)
{
	state $repo = DI->get('lore_data_repo');
	return $repo->load($self->class_id);
}

sub _build_race ($self)
{
	state $repo = DI->get('lore_data_repo');
	return $repo->load($self->race_id);
}

__PACKAGE__->_register;

