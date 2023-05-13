package Game::Server::Role::Combat;

use My::Moose::Role;

use all 'Game::Mechanics';

use header;

requires qw(
	find_in_radius
);

sub use_ability ($self, $actor_id, %options)
{
	# TODO: check and use proper ability from $options{ability}

	my $actor = $self->location->get_actor($actor_id);

	# TODO: for now this oversimplified code hardcodes an auto attack
	my $angle = $actor->stats->angle;
	my ($x, $y) = ($actor->variables->pos_x, $actor->variables->pos_y);
	my $radius = 0.25;
	my $distance = 0.2;

	my @found = grep { $_ != $actor } Game::Mechanics::Distance->find_actors_in_range(
		$self,
		Game::Mechanics::Generic->find_frontal_point($x, $y, $angle, $distance),
		$radius
	);

	# TODO: attribute
	# TODO: calculate damage
	Game::Mechanics::Character::Damage->deal_damage('todo', 5, @found);

	# TODO: other players in range should know that the ability has taken place, so they can animate it
	# TODO: other players in range should know that players health has changed
	# the hard part: use players which have discovered each player - possibly different list for each affected player (for big AOEs)

	return;
}

