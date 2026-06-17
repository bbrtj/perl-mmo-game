package My::Moose::Role::TracksDirty;

use v5.42;
use My::Moose::Role;

# keeps extra data for instances that we want to keep very private
my %dirty;
my %always_dirty;

sub _dirty ($self, $field)
{
	$dirty{refaddr $self}{$field} = 1;

	return;
}

sub _all_dirty ($self)
{
	my @dirty = keys(($dirty{refaddr $self} // {})->%*);
	my $always_dirty = $always_dirty{ref $self} //= do {
		my @always_dirty;
		foreach my $attribute ($self->meta->get_all_attributes) {
			next unless $attribute->does('My::Moose::Trait::Attribute::Stored') && $attribute->always_dirty;
			push @always_dirty, $attribute->name;
		}

		\@always_dirty;
	};

	return (@dirty, $always_dirty->@*);
}

sub _clear_dirty ($self)
{
	$dirty{refaddr $self} = {};

	return;
}

sub DEMOLISH ($self, $global_destruction)
{
	delete $dirty{refaddr $self};

	return;
}

