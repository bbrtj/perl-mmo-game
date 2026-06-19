package Game::TestClient::Action;

use My::Moose;
use Value::Diff;

use header;

use overload
	q{""} => \&stringify,
	bool => sub { 1 },
	fallback => 1
	;

has param 'client' => (
	isa => InstanceOf ['Game::TestClient'],
	weak_ref => 1,
);

has field 'state' => (
	isa => Dict [send => ArrayRef, receive => ArrayRef],
	lazy => 1,
	clearer => 'setup_state',
);

sub _build_state ($self)
{
	return {
		send => [$self->send_queue],
		receive => [$self->receive_queue],
	};
}

sub finished ($self)
{
	my $state = $self->state;
	my @send = $state->{send}->@*;
	my @receive = $state->{receive}->@*;

	return !@send && !@receive;
}

sub should_send ($self)
{
	return false unless $self->state->{send}->@*;
	return true if $self->sequential || !$self->state->{receive}->@*;
	return false;
}

sub get_data ($self)
{
	return shift $self->state->{send}->@*;
}

sub find_and_compare ($self, $type, $data)
{
	my @rec_q = $self->state->{receive}->@*;
	foreach my $i (keys @rec_q) {
		my $expected = $rec_q[$i];
		my $expected_type = '';
		my $mangler;

		if ($expected isa 'Resource') {
			if ($expected->can('mangle_for_test')) {
				my $resource = $expected;
				$mangler = sub { $resource->mangle_for_test(@_) };
			}

			$expected_type = $expected->type;
			if ($expected->is_plaintext) {
				$expected = $expected->serialized;
			}
			else {
				$expected = $expected->generate;
			}
		}

		my $cmp_data = $data;
		my $ok = 1;
		try {
			$cmp_data = __deserialize($cmp_data)
				if ref $expected;
		}
		catch ($e) {
			$ok = 0;
		}

		$ok &&= $type eq $expected_type;

		if ($ok && $mangler) {
			$expected = $mangler->($cmp_data, $expected);
		}

		$ok &&= !diff($cmp_data, $expected) && !diff($expected, $cmp_data);

		if ($ok) {
			splice $self->state->{receive}->@*, $i, 1;
			return true;
		}

		last if $self->sequential;
	}

	return false;
}

sub get_expected_type ($self)
{
	my @rec_q = $self->state->{receive}->@*;

	return 'none (queue is empty)'
		unless @rec_q;

	my $rec_count = @rec_q;
	return "one of many ($rec_count possible, queue is not sequential)"
		if !$self->sequential && $rec_count > 1;

	my $expected = $rec_q[0];

	return $expected->type
		if $expected isa 'Resource';

	return '';
}

sub get_expected_data ($self)
{
	my @rec_q = $self->state->{receive}->@*;

	return 'none (queue is empty)'
		unless @rec_q;

	my $rec_count = @rec_q;
	return "one of many ($rec_count possible, queue is not sequential)"
		if !$self->sequential && $rec_count > 1;

	my $expected = $rec_q[0];
	$expected = $expected->generate
		if $expected isa 'Resource';

	return My::Dumper->dd($expected);
}

sub BUILD ($self, @)
{
	my %requirements = map { $_ => 1 } $self->requires->@*;
	foreach my $existing_action ($self->client->actions->@*) {
		my $class = ref $existing_action;
		$class =~ s/.+::(\w+)$/$1/;

		delete $requirements{$class};
	}

	croak 'Test requirements not satisfied. Missing actions: ' . join ', ', keys %requirements
		if %requirements;

	return;
}

sub stringify ($self, @)
{
	my $class = ref $self;
	$class =~ s/^.+://;

	return $class;
}

# this part is meant to be extended

use constant requires => [];
use constant sequential => true;

sub send_queue ($self) { ... }
sub receive_queue ($self) { ... }

