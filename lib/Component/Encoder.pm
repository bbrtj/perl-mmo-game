package Component::Encoder;

use My::Moose;

use Sereal::Encoder;
use Sereal::Decoder;

use header;

has param 'encoder' => (
	isa => Types::InstanceOf ['Sereal::Encoder'],
	lazy => sub ($self) {
		Sereal::Encoder->new;
	},
	handles => [qw(encode)],
);

has param 'decoder' => (
	isa => Types::InstanceOf ['Sereal::Decoder'],
	lazy => sub ($self) {
		Sereal::Decoder->new;
	},
	handles => [qw(decode)],
);

