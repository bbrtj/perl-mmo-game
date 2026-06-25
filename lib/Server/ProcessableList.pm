package Server::ProcessableList;

use My::Moose;
use Utils qw(find_subclasses);

use header;

has field 'processable' => (
	isa => HashRef [ConsumerOf ['Server::Role::Processable']],
	builder => 1,
	'handles{}' => {
		'hash' => 'all',
		'get_by_name' => 'get',
	}
);

sub _build_processable ($self)
{
	my @classes = grep { !$_->disabled } (
		find_subclasses('Server::Action'),
		find_subclasses('Server::Job'),
		find_subclasses('Server::Event'),
	);

	return {
		map {
			$_->name => $_->new
		} @classes
	};
}

sub get_by_type_and_name ($self, $type, $name)
{
	my $by_name = $self->get_by_name($name);
	my ($by_type) = $self->get_by_type($type, $name => $by_name);

	return $by_type;
}

sub get_by_type ($self, $type, %hash)
{
	%hash = $self->hash if !%hash;
	return grep { $_ isa $type } values %hash;
}

sub get_by_type_hash ($self, $type, %hash)
{
	return map {
		$_->name => $_
	} $self->get_by_type($type, %hash);
}

