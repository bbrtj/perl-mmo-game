package Game::Server::Role::Projectiles;

use My::Moose::Role;
use Game::Config;
use Game::Object::Projectile;
use Game::Mechanics::Generic qw(calculate_angle_and_diagonal);
use Game::Mechanics::Movement qw(move_projectile);
use Game::Mechanics::Distance qw(find_actors_in_range);
use Game::Mechanics::Combat qw(is_friendly);
use Game::Mechanics::Rng;
use Math::Trig qw(deg2rad);

use all 'X';
use all 'Resource';

use header;

requires qw(
	map
	location
	actors_collision
);

has param '_projectiles' => (
	isa => HashRef [InstanceOf ['Game::Object::Projectile']],
	default => sub { {} },
);

sub _projectile_hit ($self, $projectile, $send)
{
	if ($send) {
		$self->send_to_players(
			[$projectile->discovered_by],
			Resource::ProjectileStop->new(subject => $projectile),
		);
	}

	$self->apply_effect($projectile->effect, $projectile->xy);
	delete $self->_projectiles->{$projectile->id};
	return;
}

sub _process_projectiles ($self)
{
	# NOTE: use half of base radius, as other radius results in unnatural
	# or missed collisions
	state $projectile_radius = Game::Config->base_radius / 2;
	my $map = $self->map;
	my $elapsed = server_time;

	foreach my $projectile (values $self->_projectiles->%*) {

		# a wall has been hit
		if (!move_projectile($projectile, $map, $elapsed)) {
			$self->_projectile_hit($projectile, true);
			next;
		}

		# projectile ran out of range - no need to notify clients, since client
		# is aware of the max distance
		if ($projectile->finished) {
			$self->_projectile_hit($projectile, false);
			next;
		}

		# collision with actors
		my $actor = $projectile->actor;
		my @collision = grep { !is_friendly($actor, $_) }
			$self->actors_collision($projectile->xy, $projectile_radius)->@*;

		$self->_projectile_hit($projectile, true)
			if @collision;
	}

	return;
}

sub spawn_projectile ($self, $effect, $at_x, $at_y)
{
	my $actor = $effect->actor;
	my $projectile_data = $effect->lore->projectile;
	my ($angle) = calculate_angle_and_diagonal($actor->variables->xy, $at_x, $at_y);

	if (my $inacc = $projectile_data->{inaccuracy} / 2) {
		my $roll = 1 - $actor->rng;
		my $side = rng() <=> 0.5;
		$angle += deg2rad $roll * $inacc * $side;
	}

	# TODO: check if actor is facing the right way (compare $angle to $actor->stats->angle)
	my $projectile = Game::Object::Projectile->new(
		effect => $effect,
		speed => $projectile_data->{speed},
		angle => $angle,
		max_distance => $projectile_data->{range},
	);

	# NOTE: data about the projectile is sent to all players who can ever see it (for all practical purposes)
	my @actors = find_actors_in_range(
		$self, $projectile->xy,
		$projectile_data->{range} * 2 + Game::Config->discover_radius
	);
	$projectile->set_discovered_by(map { $_->id } @actors);

	$self->_projectiles->{$projectile->id} = $projectile;

	$self->send_to_players(
		[$projectile->discovered_by],
		Resource::Projectile->new(subject => $projectile),
	);

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(0.1 => '_process_projectiles', 8);
};

