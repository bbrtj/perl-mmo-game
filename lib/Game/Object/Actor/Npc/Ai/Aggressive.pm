package Game::Object::Actor::Npc::Ai::Aggressive;

use My::Moose;
use Game::Mechanics::Distance qw(find_actors_in_range calculate_distance);
use Game::Mechanics::Combat qw(is_friendly);

use header;

extends 'Game::Object::Actor::Npc::Ai';

has param 'aggro_range' => (
	lax_isa => PositiveNum,
	default => sub { Game::Config->discover_radius },
);

with qw(
	Game::Object::Actor::Npc::Ai::Role::CanFight
);

sub act ($self, $server, $npc_actor, $elapsed = server_time)
{
	my $aggro = $self->parent->aggro_map;
	my @xy = $npc_actor->variables->xy;

	if (!$aggro->%*) {
		my @actors = find_actors_in_range($server, @xy, $self->aggro_range);

		my $closest;
		my $closest_distance = 'inf';
		foreach my $enemy (@actors) {
			next unless is_friendly($npc_actor, $enemy);

			my $distance = calculate_distance(@xy, $enemy->variables->xy);
			if ($distance < $closest_distance) {
				$closest = $enemy;
				$closest_distance = $distance;
			}
		}

		return unless defined $closest;
		$aggro->{$closest->id} = 1;
	}

	# TODO: stop chasing, reset aggro when enemy disappears
	$self->fight($server, $npc_actor);

	return;
}

