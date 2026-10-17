package Game::Server::Role::Stats;

use My::Moose::Role;
use Game::Config;

use Game::Mechanics::Character::Statistics qw(get_current_level);

use header;

requires qw(
	send_to_players
	get_discovered_by
);

sub _check_level ($self, $actor, @)
{
	my $level_stats = $actor->stats->level;
	my $level_exp = get_current_level($actor->variables->experience);

	# TODO: send player info about receiving experience

	if ($level_stats != $level_exp) {
		# level up

		$actor->clear_stats;

		my $resource = Resource::ActorPublicState->new(subject => $actor);
		$self->send_to_players(
			[$actor->id, $self->get_discovered_by($actor->id)],
			$resource,
		);
	}
}

after BUILD => sub ($self, @) {
	$self->_add_signal(stat_changed => '_check_level', q{$type eq 'exp'});
};

