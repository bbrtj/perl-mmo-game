use experimental 'class';

class Resource::Chat :isa(Resource);

use header;

use enum qw(SAY YELL PRIVATE SYSTEM);
use constant CHAT_TYPES => [SAY .. SYSTEM];

field $actor :param(subject);    # Unit::Actor
field $sent_to :param = undef;
field $message :param;
field $chat_type :param = SAY;    # CHAT_TYPES enum

# TODO: validate chat type?

use constant type => 'chat';

method generate ()
{
	return {
		id => $actor->id,
		message => $message,
		type => $chat_type,
		(defined $sent_to ? (sent_to => $sent_to) : ()),
	};
}

