package Game::Server;

use My::Moose;
use Game::Config;
use Server::Config;
use Sub::Quote qw(quote_sub);
use POSIX qw(round);

use header;

has injected 'env';
has injected 'cache_repo';
has injected 'lore_data_repo';

has param 'process' => (
	isa => HasMethods [qw(send_to_player send_to_players log)],
	weak_ref => 1,
	'handles->' => {
		'send_to_player' => 'send_to_player',
		'send_to_players' => 'send_to_players',
		'log' => 'log',
	},
);

has param 'location' => (
	isa => InstanceOf ['Unit::Location'],
	'handles->' => {
		'get_player' => 'get_player',
	},
);

has field 'map' => (
	lazy => sub ($self) {
		return $self->location->lore->map;
	}
);

has field '_actions' => (
	isa => ArrayRef [HashRef [ArrayRef [SimpleStr]]],
	default => sub { [] },
);

has cached '_compiled_action' => (
	isa => CodeRef,
	lazy => 1,
);

has field '_signals' => (
	isa => HashRef [ArrayRef [Tuple [SimpleStr, Maybe [Str]]]],
	default => sub { {} },
);

has cached '_compiled_signals' => (
	isa => HashRef [CodeRef],
	lazy => 1,
);

has field '_queue' => (
	isa => ArrayRef [ArrayRef],
	default => sub { [] },
	'handles[]' => {
		'_clear_queue' => 'clear',
	},
);

with qw(
	Game::Server::Role::QuadTree
	Game::Server::Role::ActionQueue
	Game::Server::Role::Discovery

	Game::Server::Role::Chat
	Game::Server::Role::Projectiles
	Game::Server::Role::Combat
	Game::Server::Role::Movements
	Game::Server::Role::Regeneration
	Game::Server::Role::Npcs
);

use constant SIGNALS => {
	player_left => '$actor',
	actor_appeared => '$for_actor, $actor',
	actor_died => '$actor',
	projectile_appeared => '$for_actor, $projectile',
};

sub _add_action ($self, $every, $handler, $priority = 0)
{
	croak "$handler is not a proper method name in " . __PACKAGE__
		unless $self->can($handler);

	# $every is time - turn it into a server tick count
	$every = round($every / Server::Config::TICK);
	$every ||= 1;

	push $self->_actions->[$priority]{$every}->@*, $handler;
	return;
}

sub _build_compiled_action ($self)
{
	my @actions_lines = (q[my ($elapsed) = @_;]);

	# high priority means actions come sooner
	for my $actions (reverse $self->_actions->@*) {
		next unless defined $actions;

		my @sorted =
			map { $_ => $actions->{$_} }
			sort { $a <=> $b }
			keys $actions->%*;

		foreach my ($every, $handlers) (@sorted) {
			push @actions_lines,
				qq[if (\$elapsed % $every == 0) {],
				(map { qq[ \$self->$_();] } $handlers->@*),
				qq[}];
		}
	}

	my $compiled = join "\n", @actions_lines;
	return quote_sub $compiled, {
		'$self' => \$self,
		}, {
			no_defer => 1,
		};
}

sub _add_signal ($self, $name, $handler, $condition = undef)
{
	croak "$name is not a proper signal name"
		unless SIGNALS->{$name};
	croak "$handler is not a proper method name in " . __PACKAGE__
		unless $self->can($handler);

	push $self->_signals->{$name}->@*, [$handler, $condition];
	return;
}

sub _build_compiled_signals ($self)
{
	my %signals;
	foreach my ($name, $consumers) ($self->_signals->%*) {
		my $signal_args = SIGNALS->{$name};
		my @handler_lines = (qq[my ($signal_args) = \@_;]);

		foreach my $consumer ($consumers->@*) {
			my ($handler, $condition) = $consumer->@*;

			$handler = qq[\$self->$handler(\@_);];
			if ($condition) {
				$handler = qq[if ($condition) { $handler }];
			}

			push @handler_lines, $handler;
		}

		my $compiled = join "\n", @handler_lines;
		$signals{$name} = quote_sub $compiled, {
			'$self' => \$self,
			}, {
				no_defer => 1,
			};
	}

	return \%signals;
}

sub BUILD ($self, $args)
{
	$self->_add_signal(player_left => '_cleanup_leaving_player');
	$self->_add_signal(actor_died => '_process_actor_death');
}

sub tick ($self, $elapsed)
{
	$self->_compiled_action->($elapsed);

	return;
}

sub queue ($self, @data)
{
	push $self->_queue->@*, \@data;

	return;
}

sub resolve_queue ($self)
{
	state $method_map = {};

	foreach my $item ($self->_queue->@*) {
		my ($name, @args) = $item->@*;
		my $method = $method_map->{$name} // $self->can($name);

		$self->$method(@args);
	}

	$self->_clear_queue;

	return;
}

sub apply_effect ($self, $effect, @args)
{
	my $method = $effect->server_method;
	return $self->$method($effect, @args);
}

sub signal ($self, $name, @args)
{
	my $signal_sub = $self->_compiled_signals->{$name} // croak "$name is not a proper signal name";
	$signal_sub->(@args);

	return;
}

sub _cleanup_leaving_player ($self, $actor)
{
	$self->location->remove_actor($actor);

	return;
}

sub _process_actor_death ($self, $actor)
{
	$self->location->remove_actor($actor);

	# TODO: player death

	return;
}

