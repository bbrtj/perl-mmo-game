package Game::Lore;

use My::Moose;

use header;

has param 'id' => (
	isa => Types::Str,
);

has param 'name' => (
	isa => Types::Str,
);

my %data_collection;

around BUILDARGS => sub ($orig, $self, %args) {
	$args{id} = join '.', $self->prefix, $args{id}
		if $args{id};
	return $self->$orig(%args);
};

sub BUILD ($self, @)
{
	state $repo = DI->get('lore_data_repo');
	$repo->save($self);
	return;
}

sub data ($self)
{
	my $id = $self->id;

	if (!exists $data_collection{$id}) {
		my $target_class = blessed($self) . 'Data';
		$data_collection{$id} = $target_class->new(main_obj => $self);
	}

	return $data_collection{$id};
}

sub prefix ($self)
{
	die 'Game::Lore has no prefix - needs a subclass';
}

