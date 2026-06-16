package Game::Object::Actor::Npc::Ai::Wander;

use My::Moose;
use Game::RNG qw(random_number);
use Game::Mechanics::Generic;
use Math::Trig qw(pi);

use header;

extends 'Game::Object::Actor::Npc::Ai';

use constant EVERY => [2, 4];
use constant DISTANCE => [0.25, 0.75];

has param 'last_wander' => (
	lax_isa => Num,
	writer => 1,
	default => sub { time },
);

sub act ($self, $server, $actor, $elapsed = server_time)
{
	return unless $elapsed >= $self->last_wander;

	my $angle = random_number 0, 2 * pi;
	my $distance = random_number DISTANCE->@*;
	my @point = Game::Mechanics::Generic->find_frontal_point($actor->variables->xy, $angle, $distance);
	$self->set_last_wander($elapsed + random_number EVERY->@*);

	$server->set_movement($actor->id, @point);
	return;
}

