package Model::Role::Stored;

use My::Moose::Role;
use Types;

use header;

with qw(
	My::Moose::Role::TracksDirty
	Model::Role::Identified
);
