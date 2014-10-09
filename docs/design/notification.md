% Retcon Notifications

Most uses of retcon require some form of human intervention to resolve
conflicts when they occur. This document describes the design of the
notification system. It is currently **draft**.

Requirements
------------

- Notifications are set for all merges with conflicts.

- Notifications are dispatched according to a set of rules.

- Each rule is something like a "report"; it describes the conditions which
will be matched against a notification (entity name, etc.) and the target for
the notification (email address, any aggregation, etc.)

- The system runs periodically under cron and processes all unprocessed
notifications according to the current set of rules.

Design
------

The system records the details of notifications in the `retcon_notification`
database table during normal operation. These records will later be processed
and deleted by a message-dispatch process. This table acts as a queue and
decouples the operation of the system from the system of email messages.

Each notification record contains the entity and internal key of the object
being processed, the ID of the diff with merge conflicts, and a time stamp for
the notification.

The message-dispatch process periodically queries this table for outstanding
notifications: each rule results in a query being performed to find all
matching records, messages are sent according to the rule's message parameters.

Notifications are recorded only when enabled by the configuration file and/or
command-line arguments.

Implementation
--------------

At each run, the current time stamp is determined and all operations on the
database restrict results using this value. This helps to avoid skipping and
double handling of notifications.

Each rule results in a query (with appropriate parameters) being executed
against the notification table; the results are processed and messages
dispatched with the target described in the rule. This allows a single
notification to be sent to multiple destinations.

Individual messages are dispatched in chronological order; and use the
notification time stamp to set the Date header. This makes it easier for
recipients to inspect a sequence of events and understand the operation of the
system.

Update the `recordDiffs` operation to update `retcon_notification` when
required.

Queries to select and update the notifications table:

````{.sql}
-- SELECT messages to be processed.
SELECT * FROM retcon_notification
WHERE (processed = FALSE) AND (created < ?) AND (entity = ?)
ORDER BY (entity, datasource) ASC, timestamp DESC

-- UPDATE messages which we just processed.
UPDATE retcon_notification
SET processed = TRUE
WHERE (processed = FALSE) AND (created < ?) AND (entity = ?);
````
