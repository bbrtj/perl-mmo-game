package Factory::User;

use My::Moose;
use Unit::User;
use Unit::Nested::Player;

use header;

sub create ($self, $user_result)
{
	my @player_results = $user_result->players;
	my @players = map { $self->_create_player($_) } @player_results;

	return Unit::User->new(
		user => $user_result->to_model,
		players => \@players,
	);
}

sub _create_player ($self, $player_result)
{
	return Unit::Nested::Player->new(
		player => $player_result->to_model,
		character => $player_result->character->to_model,
	);
}

