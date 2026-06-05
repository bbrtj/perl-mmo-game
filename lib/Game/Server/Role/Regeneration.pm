package Game::Server::Role::Regeneration;

use My::Moose::Role;
use Game::Config;
use List::Util qw(min);

use all 'X';
use all 'Resource';

use header;

requires qw(
	location
);

has field '_last_regeneration_tick' => (
	lax_isa => PositiveNum,
	writer => 1,
);

sub _apply_regeneration ($self)
{
	my $time = server_time;
	my $elapsed = $time - ($self->_last_regeneration_tick // $time);
	$self->_set_last_regeneration_tick($time);

	foreach my $actor (values $self->location->actors->%*) {
		my $variables = $actor->variables;
		my $stats = $actor->stats;

		# TODO: apply status bonuses (sitting? eating?)
		# TODO: skip dead?
		# TODO: move to Game::Mechanics
		$variables->set_health(min($stats->max_health, $variables->health + $stats->health_regeneration * $elapsed));
		$variables->set_energy(min($stats->max_energy, $variables->energy + $stats->energy_regeneration * $elapsed));
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_apply_regeneration')
		unless $self->env->getenv('TEST_NO_REGENERATION');
};

