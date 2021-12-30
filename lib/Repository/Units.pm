package Repository::Units;

use My::Moose;
use Factory::Actor;
use Factory::Battle;
use Factory::User;
use Types;

use header;

has 'db' => (
	is => 'ro',
);

has 'repo' => (
	is => 'ro',
);

# TODO: check if queries actually fetched anything

sub save ($self, $unit)
{
	state $check = Types::InstanceOf ['Unit'];
	$check->assert_valid($unit);

	$self->repo->update($_) for $unit->models->@*;
	return;
}

sub get_actor ($self, $character_id)
{
	my $result = $self->db->dbc->resultset('Character')->search(
		{'me.id' => $character_id},
		{
			prefetch => [qw(player variables contestant)],
		}
	)->single;

	return Factory::Actor->create($result);
}

sub get_battle ($self, $battle_id)
{
	my $result = $self->db->dbc->resultset('Battle')->search(
		{'battle.id' => $battle_id},
		{
			prefetch => {contestants => {character => [qw(player variables contestant)]}}
		}
	)->single;

	return Factory::Actor->create($result);
}

sub get_user ($self, $user_id)
{
	my $result = $self->db->dbc->resultset('User')->search(
		{'user.id' => $user_id},
		{
			prefetch => {players => [qw(character)]}
		}
	)->single;

	return Factory::User->create($result);
}
