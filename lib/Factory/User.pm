package Factory::User;

use My::Moose;
use Unit::User;
use Unit::Nested::Player;
use Schema::Utils qw(fetch_single);

use header;

extends 'Factory';

sub fetch ($self, $user_id)
{
	my $rs = $self->dbc->resultset('User')->search(
		{'me.id' => $user_id},
		{
			prefetch => {players => [qw(character)]}
		}
	);

	return fetch_single($rs);
}

sub create ($self, $user_result)
{
	my @player_results = $user_result->players;

	return Unit::User->new(
		user => $user_result->to_model,
		players => [map { $self->_create_player($_) } @player_results],
	);
}

sub _create_player ($self, $player_result)
{
	return Unit::Nested::Player->new(
		player => $player_result->to_model,
		character => $player_result->character->to_model,
	);
}

