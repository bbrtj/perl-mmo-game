package Component::Log;

use Types;
use My::Moose;
use Log::Dispatch;
use Time::Piece;

use header;

with 'Component::Role::HasEnv';

has 'logger' => (
	is => 'ro',
	isa => Types::InstanceOf ['Log::Dispatch'],
	lazy => 1,
	default => sub ($self) {
		Log::Dispatch->new(outputs => $self->build_config);
	},
	handles => [qw(debug info warning error critical emergency)],
);

has 'filename' => (
	is => 'rw',
	isa => Types::Str,
);

has 'system_name' => (
	is => 'rw',
	isa => Types::Str,
);

sub _get_log_callback ($self)
{
	return sub (%params) {
		my $time = localtime;
		my $time_str = $time->ymd . ' ' . $time->hms;

		my $str = "[$time_str] ";
		my $level_str = uc $params{level};
		chomp $params{message};
		my $ph = " " x length $str;
		$params{message} =~ s/(\R)/$1$ph\[$level_str] /g;

		return "$str\[$level_str] $params{message}\n";
	};
}

sub _get_screen_callback ($self)
{
	return sub (%params) {
		my $time = localtime;
		my $time_str = $time->hms;
		my $sys_str = $self->system_name // '';

		$sys_str = "[$sys_str] " if $sys_str;
		my $str = "[$time_str] $sys_str";
		my $level_str = uc $params{level};
		chomp $params{message};
		my $ph = " " x length $str;
		$params{message} =~ s/(\R)/$1$ph\[$level_str] /g;

		return "$str\[$level_str] $params{message}\n";
	};
}

sub build_config ($self)
{

	return [
		($self->env->is_production ? () : ([
			'Screen',
			name => 'development debug',
			min_level => 'debug',
			max_level => 'critical',
			stderr => 0,
			callbacks => $self->_get_screen_callback,
		])),
		[
			'File::Locked',
			name => 'file',
			min_level => 'warning',
			filename => $self->filename,
			mode => '>>',
			binmode => ":encoding(UTF-8)",
			callbacks => $self->_get_log_callback,
		],
		[
			'Screen',
			name => 'stderr',
			min_level => 'emergency',
			stderr => 1,
			callbacks => $self->_get_log_callback,
		],
	];
}

