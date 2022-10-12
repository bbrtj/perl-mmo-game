package Model::Role::Stored;

use My::Moose::Role;

use header;

with qw(
	My::Moose::Role::TracksDirty
	Model::Role::Identified
);

