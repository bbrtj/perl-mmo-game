package Repository::Units;

use My::Moose;
use Factory::Actor;
use Factory::User;
use Types;
use Schema::Utils qw(fetch_single);

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
	my $rs = $self->db->dbc->resultset('Character')->search(
		{'me.id' => $character_id},
		{
			prefetch => [qw(player variables)],
		}
	);

	return Factory::Actor->create(fetch_single($rs));
}

sub get_user ($self, $user_id)
{
	my $rs = $self->db->dbc->resultset('User')->search(
		{'me.id' => $user_id},
		{
			prefetch => {players => [qw(character)]}
		}
	);

	return Factory::User->create(fetch_single($rs));
}

