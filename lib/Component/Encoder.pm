package Component::Encoder;

use My::Moose;

use Sereal::Encoder;
use Sereal::Decoder;

use header;

has 'encoder' => (
	is => 'ro',
	isa => Types::InstanceOf ['Sereal::Encoder'],
	lazy => 1,
	default => sub ($self) {
		Sereal::Encoder->new;
	},
	handles => [qw(encode)],
);

has 'decoder' => (
	is => 'ro',
	isa => Types::InstanceOf ['Sereal::Decoder'],
	lazy => 1,
	default => sub ($self) {
		Sereal::Decoder->new;
	},
	handles => [qw(decode)],
);

