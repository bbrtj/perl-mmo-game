package Game::Server::Role::ActionQueue;

use My::Moose::Role;

use My::PQ::Elem;
use My::PQ;

use header;

has field 'queued_actions' => (
	default => sub { My::PQ->new },
);

sub enqueue_action ($self, $action)
{
	$self->queued_actions->add(My::PQ::Elem->new(val => $action, cmp_val => $action->eta));
	return;
}

sub _process_actions ($self)
{
	my $queue = $self->queued_actions;
	my $time = server_time;
	my $el;

	while (($el = $queue->top) && $el->val->finished($time)) {
		my $action = $el->val;
		$queue->extract_top;

		next if $action->cancelled;

		my $method = $action->server_method;
		$self->$method($action);
	}
}

after BUILD => sub ($self, @) {
	$self->_add_action(0.05 => '_process_actions');
};

after signal_player_left => sub ($self, $actor) {

	# TODO: dequeue all actor actions
};

