package Game::TestClient::Action::State;

use My::Moose;
use Data::Dumper;

use header;

extends 'Game::TestClient::Action';

has param 'state' => (
	isa => Types::HashRef,
	writer => 1,
);

use constant sequential => !!0;
use constant requires => ['EnterGame'];

sub send_queue ($self)
{
	return ();
}

sub receive_queue ($self)
{
	return ();
}

sub finished ($self)
{
	return !$self->state->%*;
}

sub should_send ($self)
{
	return !!0;
}

my $no_diff = \'no_diff';

sub _diff_hash ($self, $left, $right)
{
	my %out;
	for my ($key, $value) ($left->%*) {
		$out{$key} = $value
			unless exists $right->{$key};

		my $diff = $self->_diff($value, $right->{$key});
		$out{$key} = $diff
			unless $diff eq $no_diff;
	}
	return %out ? \%out : $no_diff;
}

sub _diff_array ($self, $left, $right)
{
	my @out;
	my @other = $right->@*;

	OUTER:
	for my $value ($left->@*) {
		for my $key (keys @other) {
			my $other_value = $other[$key];
			my $diff = $self->_diff($value, $other_value);
			if ($diff eq $no_diff) {
				splice @other, $key, 1;
				next OUTER;
			}
		}

		push @out, $value;
	}

	return @out ? \@out : $no_diff;
}

sub _diff ($self, $left, $right)
{
	my $ref_left = ref $left;
	my $ref_right = ref $right;
	return $left if $ref_left ne $ref_right;
	return _diff_array if $ref_left eq 'ARRAY';
	return _diff_hash if $ref_left eq 'HASH';

	croak "cannot compare references to $ref_left"
		if length $ref_left;

	return $left
		if defined $left ne defined $right
		|| (defined $left && $left ne $right);

	return $no_diff;
}

sub diff ($self, $left, $right, $out)
{
	my $diff = $self->_diff($left, $right);

	if ($diff eq $no_diff) {
		return !!0;
	}
	else {
		\$out = $diff;
		return !!1;
	}
}

sub find_and_compare ($self, $data)
{
	my $diff;
	if ($self->diff($data, $self->state, \$diff)) {
		return !!0;
	}
	elsif ($self->diff($self->state, $data, \$diff)) {
		$self->set_state($diff);
	}
	else {
		$self->set_state({});
	}

	return !!1;
}

sub get_expected_data ($self)
{
	return Dumper($self->state);
}

