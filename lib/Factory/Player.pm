package Factory::Player;

use My::Moose;
use Unit::Player;

use header;

sub create ($self, $player_result)
{
	return Unit::Player->new(
		player => $player_result->to_model,
		character => $player_result->character->to_model,
	);
}


