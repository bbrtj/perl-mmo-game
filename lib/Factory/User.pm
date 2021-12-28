package Factory::User;

use My::Moose;
use Unit::User;
use Factory::Player;

use header;

sub create ($self, $user_result)
{
	my @player_results = $user_result->players->@*;
	my @players = map { Factory::Player->create($_->player) } @player_results;

	return Unit::User->new(
		user => $user_result->to_model,
		players => \@players,
	);
}

