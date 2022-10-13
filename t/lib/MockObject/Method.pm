package MockObject::Method;

use My::Moose;

use header;

has field 'called_times' => (
	writer => -hidden,
	clearer => -hidden,
	lazy => sub { 0 },
);

has field 'call_history' => (
	clearer => -hidden,
	lazy => sub { [] },
);

has field '_throws' => (
	writer => 1,
	clearer => 1,
);

has field '_calls' => (
	writer => 1,
	clearer => 1,
);

has field '_returns' => (
	writer => 1,
	clearer => 1,
);

has field '_returns_list' => (
	writer => 1,
	clearer => 1,
);


sub called_with ($self)
{
	return $self->last_called_with;
}

sub first_called_with ($self)
{
	return $self->call_history->[0];
}

sub last_called_with ($self)
{
	return $self->call_history->[-1];
}

sub was_called ($self)
{
	return $self->called_times > 0;
}

sub was_called_once ($self)
{
	return $self->was_called_times(1);
}

sub was_called_times ($self, $times)
{
	return $self->called_times == $times;
}

sub clear ($self)
{
	$self->_clear_called_times;
	$self->_clear_call_history;

	return $self;
}

sub _forget ($self)
{
	$self->_clear_returns;
	$self->_clear_returns_list;
	$self->_clear_calls;
	$self->_clear_throws;

	return;
}

sub should_return ($self, @values)
{
	$self->_forget;

	if (@values == 1) {
		$self->_set_returns($values[0]);
	}
	else {
		$self->_set_returns_list([@values]);
	}

	return $self->clear;
}

sub should_call ($self, $sub)
{
	croak 'should_call expects a coderef'
		unless ref $sub eq 'CODE';

	$self->_forget;

	$self->_set_calls($sub);

	return $self->clear;
}

sub should_throw ($self, $exception)
{
	$self->_forget;

	$self->_set_throws($exception);

	return $self->clear;
}

sub _called ($self, $inner_self, @params)
{
	$self->_set_called_times($self->called_times + 1);
	push $self->call_history->@*, [@params];

	die $self->_throws
		if defined $self->_throws;

	return $self->_calls->($inner_self, @params)
		if $self->_calls;

	return $self->_returns_list->@*
		if $self->_returns_list;

	return $self->_returns;
}

