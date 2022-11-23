package Game::TestClient::Action;

use My::Moose;
use Mojo::JSON qw(from_json to_json);
use Data::Compare;
use Data::Dumper;

use header;

use overload
	q{""} => \&stringify,
	bool => sub { 1 },
	fallback => 1
	;

has param 'client' => (
	isa => Types::InstanceOf ['Game::TestClient'],
	weak_ref => 1,
);

has field 'state' => (
	isa => Types::Dict [send => Types::ArrayRef, receive => Types::ArrayRef],
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
	return !!0 unless $self->state->{send}->@*;
	return !!1 if $self->sequential || !$self->state->{receive}->@*;
	return !!0;
}

sub get_data ($self)
{
	return shift $self->state->{send}->@*;
}

sub find_and_compare ($self, $data)
{
	my @rec_q = $self->state->{receive}->@*;
	foreach my $i (keys @rec_q) {
		my $expected = $rec_q[$i];
		$expected = $expected->serialize
			if $expected isa 'Resource';

		my $cmp_data = $data;
		$cmp_data = from_json($cmp_data)
			if is_ref $expected;

		my $ok = Compare($cmp_data, $expected);
		if ($ok) {
			splice $self->state->{receive}->@*, $i, 1;
			return !!1;
		}

		last if $self->sequential;
	}

	return !!0;
}

sub get_expected_data ($self)
{
	my @rec_q = $self->state->{receive}->@*;

	return 'none (queue is empty)'
		unless @rec_q;

	return 'one of many (queue is not sequential)'
		if !$self->sequential && @rec_q > 1;

	my $expected = $rec_q[0];
	$expected = $expected->serialize
		if $expected isa 'Resource';

	return Dumper($expected);
}

sub encode ($self, $data)
{
	return to_json($data);
}

sub decode ($self, $data)
{
	return from_json($data);
}

sub BUILD ($self, @)
{
	my %requirements = map { $_ => 1 } $self->requires->@*;
	foreach my $existing_action ($self->client->actions->@*) {
		my $class = ref $existing_action;
		$class =~ s/.+::(\w+)$/$1/;

		delete $requirements{$class};
	}

	croak 'Test requirements not satisfied. Missing actions: ' . keys %requirements
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
use constant sequential => !!1;

sub send_queue ($self) { ... }
sub receive_queue ($self) { ... }

