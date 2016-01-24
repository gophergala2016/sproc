package sproc

import (
	"bufio"
	"container/list"
	"fmt"
	"io"
)

type StreamDirection int

const (
	StreamInput StreamDirection = 1 << iota
	StreamOutput

	StreamDuplex = StreamInput | StreamOutput
)

const (
	StreamBuf = 8
)

type Stream struct {
	items *list.List
	sends chan *StreamItem
	recvs chan *StreamItem
}

func NewStream() *Stream {
	str := &Stream{list.New(), make(chan *StreamItem, StreamBuf), make(chan *StreamItem, StreamBuf)}
	go str.shovel()
	return str
}

func (s *Stream) In() *StreamSlot {
	return &StreamSlot{s, StreamInput}
}

// FIXME: we want to have fanouts (FanOut method?)
func (s *Stream) Out() *StreamSlot {
	return &StreamSlot{s, StreamOutput}
}

func (s *Stream) shovel() {
	deadStop := make(chan *StreamItem)
	dsend := s.sends
	drecv := deadStop

	var transit *StreamItem
	for {
		if transit == nil {
			drecv = deadStop
		} else {
			drecv = s.recvs
		}

		select {
		case item := <-dsend:
			if transit == nil {
				transit = item
			} else {
				s.items.PushBack(item)
			}
		case drecv <- transit:
			if s.items.Len() == 0 {
				transit = nil
			} else {
				transit = s.items.Remove(s.items.Front()).(*StreamItem)
			}
		}
	}
}

func (s *Stream) send(item *StreamItem) {
	s.sends <- item
}

func (s *Stream) recv() (*StreamItem, bool) {
	item, ok := <-s.recvs
	return item, ok
}

type StreamSlot struct {
	stream    *Stream
	direction StreamDirection
}

func (slot *StreamSlot) Send(item *StreamItem) {
	if slot.direction&StreamInput == 0 {
		panic("this end of stream does not accept input")
	}

	slot.stream.send(item)
}

func (slot *StreamSlot) Recv() (*StreamItem, bool) {
	if slot.direction&StreamOutput == 0 {
		panic("this end of stream does not produce output")
	}

	return slot.stream.recv()
}

func (slot *StreamSlot) Direction() StreamDirection {
	return slot.direction
}

type StreamItem struct {
	values *VarList // stack of values
}

func NewStreamItem() *StreamItem {
	return &StreamItem{}
}

// main value represents item as a whole
// e.g. item can have supplementary variables but as a whole will be a line
// or number
// this value will be printed out
func (si *StreamItem) FindMainValue() *Value {
	if si == nil {
		panic("stream item is nil")
	}

	vr := si.values.Lookup("")
	if vr == nil {
		return nil
	}

	return vr.value
}

func (si *StreamItem) AddValue(name string, value *Value) {
	vr := &Variable{name, value.Copy()}
	si.values = AddVar(vr, si.values)
}

type IOBase struct {
	stdin, stdout *StreamSlot
}

func (ib *IOBase) UseStreams(in, out *StreamSlot) {
	ib.stdin = in
	ib.stdout = out
}

func (ib *IOBase) InheritIO(b *IOBase) {
	*ib = *b
}

func ReadLinesToStream(reader io.Reader, st *StreamSlot) error {
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return err
		}

		val := &Value{typ: TypeString, stringVal: scanner.Text()}
		item := NewStreamItem()
		item.AddValue("", val)

		st.Send(item)
	}

	return nil
}

func WriteLinesFromStream(writer io.Writer, st *StreamSlot) {
	for {
		item, ok := st.Recv()
		if !ok {
			return
		}

		val := item.FindMainValue()
		if val == nil {
			println("writer: no main value in stream")
			continue
		}

		switch val.typ {
		case TypeString:
			fmt.Fprintln(writer, val.stringVal)
		case TypeInt:
			fmt.Fprintln(writer, val.intVal)
		case TypeFloat:
			fmt.Fprintln(writer, val.floatVal)
		default:
			fmt.Fprintln(writer, "<unprintable type>")
		}
	}
}
