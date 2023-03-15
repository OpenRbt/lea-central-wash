// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.25.0-devel
// 	protoc        v3.14.0
// source: hal.proto

package xgrpc

import (
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	emptypb "google.golang.org/protobuf/types/known/emptypb"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type Options struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	StationId         int32    `protobuf:"varint,1,opt,name=station_id,json=stationId,proto3" json:"station_id,omitempty"`
	TimeoutSec        int32    `protobuf:"varint,2,opt,name=timeoutSec,proto3" json:"timeoutSec,omitempty"`
	MotorSpeedPercent int32    `protobuf:"varint,3,opt,name=motorSpeedPercent,proto3" json:"motorSpeedPercent,omitempty"`
	Relays            []*Relay `protobuf:"bytes,4,rep,name=relays,proto3" json:"relays,omitempty"`
}

func (x *Options) Reset() {
	*x = Options{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Options) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Options) ProtoMessage() {}

func (x *Options) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Options.ProtoReflect.Descriptor instead.
func (*Options) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{0}
}

func (x *Options) GetStationId() int32 {
	if x != nil {
		return x.StationId
	}
	return 0
}

func (x *Options) GetTimeoutSec() int32 {
	if x != nil {
		return x.TimeoutSec
	}
	return 0
}

func (x *Options) GetMotorSpeedPercent() int32 {
	if x != nil {
		return x.MotorSpeedPercent
	}
	return 0
}

func (x *Options) GetRelays() []*Relay {
	if x != nil {
		return x.Relays
	}
	return nil
}

type Relay struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	ID      int32 `protobuf:"varint,1,opt,name=ID,proto3" json:"ID,omitempty"`
	TimeOn  int32 `protobuf:"varint,2,opt,name=TimeOn,proto3" json:"TimeOn,omitempty"`
	TimeOff int32 `protobuf:"varint,3,opt,name=TimeOff,proto3" json:"TimeOff,omitempty"`
}

func (x *Relay) Reset() {
	*x = Relay{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Relay) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Relay) ProtoMessage() {}

func (x *Relay) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Relay.ProtoReflect.Descriptor instead.
func (*Relay) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{1}
}

func (x *Relay) GetID() int32 {
	if x != nil {
		return x.ID
	}
	return 0
}

func (x *Relay) GetTimeOn() int32 {
	if x != nil {
		return x.TimeOn
	}
	return 0
}

func (x *Relay) GetTimeOff() int32 {
	if x != nil {
		return x.TimeOff
	}
	return 0
}

type OptionsCommand struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Volume                 int32    `protobuf:"varint,1,opt,name=volume,proto3" json:"volume,omitempty"`
	StationId              int32    `protobuf:"varint,2,opt,name=station_id,json=stationId,proto3" json:"station_id,omitempty"`
	StartTimeoutSec        int32    `protobuf:"varint,3,opt,name=StartTimeoutSec,proto3" json:"StartTimeoutSec,omitempty"`
	StartMotorSpeedPercent int32    `protobuf:"varint,4,opt,name=StartMotorSpeedPercent,proto3" json:"StartMotorSpeedPercent,omitempty"`
	StartRelays            []*Relay `protobuf:"bytes,5,rep,name=StartRelays,proto3" json:"StartRelays,omitempty"`
	StopTimeoutSec         int32    `protobuf:"varint,6,opt,name=StopTimeoutSec,proto3" json:"StopTimeoutSec,omitempty"`
	StopMotorSpeedPercent  int32    `protobuf:"varint,7,opt,name=StopMotorSpeedPercent,proto3" json:"StopMotorSpeedPercent,omitempty"`
	StopRelays             []*Relay `protobuf:"bytes,8,rep,name=StopRelays,proto3" json:"StopRelays,omitempty"`
}

func (x *OptionsCommand) Reset() {
	*x = OptionsCommand{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[2]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *OptionsCommand) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*OptionsCommand) ProtoMessage() {}

func (x *OptionsCommand) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[2]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use OptionsCommand.ProtoReflect.Descriptor instead.
func (*OptionsCommand) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{2}
}

func (x *OptionsCommand) GetVolume() int32 {
	if x != nil {
		return x.Volume
	}
	return 0
}

func (x *OptionsCommand) GetStationId() int32 {
	if x != nil {
		return x.StationId
	}
	return 0
}

func (x *OptionsCommand) GetStartTimeoutSec() int32 {
	if x != nil {
		return x.StartTimeoutSec
	}
	return 0
}

func (x *OptionsCommand) GetStartMotorSpeedPercent() int32 {
	if x != nil {
		return x.StartMotorSpeedPercent
	}
	return 0
}

func (x *OptionsCommand) GetStartRelays() []*Relay {
	if x != nil {
		return x.StartRelays
	}
	return nil
}

func (x *OptionsCommand) GetStopTimeoutSec() int32 {
	if x != nil {
		return x.StopTimeoutSec
	}
	return 0
}

func (x *OptionsCommand) GetStopMotorSpeedPercent() int32 {
	if x != nil {
		return x.StopMotorSpeedPercent
	}
	return 0
}

func (x *OptionsCommand) GetStopRelays() []*Relay {
	if x != nil {
		return x.StopRelays
	}
	return nil
}

type Answer struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Answer int64  `protobuf:"varint,1,opt,name=answer,proto3" json:"answer,omitempty"`
	Status string `protobuf:"bytes,2,opt,name=status,proto3" json:"status,omitempty"`
}

func (x *Answer) Reset() {
	*x = Answer{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[3]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Answer) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Answer) ProtoMessage() {}

func (x *Answer) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[3]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Answer.ProtoReflect.Descriptor instead.
func (*Answer) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{3}
}

func (x *Answer) GetAnswer() int64 {
	if x != nil {
		return x.Answer
	}
	return 0
}

func (x *Answer) GetStatus() string {
	if x != nil {
		return x.Status
	}
	return ""
}

type AnswerCommand struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Answer int64 `protobuf:"varint,1,opt,name=answer,proto3" json:"answer,omitempty"`
}

func (x *AnswerCommand) Reset() {
	*x = AnswerCommand{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[4]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *AnswerCommand) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*AnswerCommand) ProtoMessage() {}

func (x *AnswerCommand) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[4]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use AnswerCommand.ProtoReflect.Descriptor instead.
func (*AnswerCommand) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{4}
}

func (x *AnswerCommand) GetAnswer() int64 {
	if x != nil {
		return x.Answer
	}
	return 0
}

type AnswerProgram struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Answer int64 `protobuf:"varint,1,opt,name=answer,proto3" json:"answer,omitempty"`
}

func (x *AnswerProgram) Reset() {
	*x = AnswerProgram{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[5]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *AnswerProgram) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*AnswerProgram) ProtoMessage() {}

func (x *AnswerProgram) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[5]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use AnswerProgram.ProtoReflect.Descriptor instead.
func (*AnswerProgram) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{5}
}

func (x *AnswerProgram) GetAnswer() int64 {
	if x != nil {
		return x.Answer
	}
	return 0
}

type AnswerLevel struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Answer int64 `protobuf:"varint,1,opt,name=answer,proto3" json:"answer,omitempty"`
}

func (x *AnswerLevel) Reset() {
	*x = AnswerLevel{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[6]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *AnswerLevel) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*AnswerLevel) ProtoMessage() {}

func (x *AnswerLevel) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[6]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use AnswerLevel.ProtoReflect.Descriptor instead.
func (*AnswerLevel) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{6}
}

func (x *AnswerLevel) GetAnswer() int64 {
	if x != nil {
		return x.Answer
	}
	return 0
}

type RequestStopDispenser struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	StationId         int32    `protobuf:"varint,1,opt,name=station_id,json=stationId,proto3" json:"station_id,omitempty"`
	TimeoutSec        int32    `protobuf:"varint,2,opt,name=timeoutSec,proto3" json:"timeoutSec,omitempty"`
	MotorSpeedPercent int32    `protobuf:"varint,3,opt,name=motorSpeedPercent,proto3" json:"motorSpeedPercent,omitempty"`
	Relays            []*Relay `protobuf:"bytes,4,rep,name=relays,proto3" json:"relays,omitempty"`
}

func (x *RequestStopDispenser) Reset() {
	*x = RequestStopDispenser{}
	if protoimpl.UnsafeEnabled {
		mi := &file_hal_proto_msgTypes[7]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *RequestStopDispenser) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*RequestStopDispenser) ProtoMessage() {}

func (x *RequestStopDispenser) ProtoReflect() protoreflect.Message {
	mi := &file_hal_proto_msgTypes[7]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use RequestStopDispenser.ProtoReflect.Descriptor instead.
func (*RequestStopDispenser) Descriptor() ([]byte, []int) {
	return file_hal_proto_rawDescGZIP(), []int{7}
}

func (x *RequestStopDispenser) GetStationId() int32 {
	if x != nil {
		return x.StationId
	}
	return 0
}

func (x *RequestStopDispenser) GetTimeoutSec() int32 {
	if x != nil {
		return x.TimeoutSec
	}
	return 0
}

func (x *RequestStopDispenser) GetMotorSpeedPercent() int32 {
	if x != nil {
		return x.MotorSpeedPercent
	}
	return 0
}

func (x *RequestStopDispenser) GetRelays() []*Relay {
	if x != nil {
		return x.Relays
	}
	return nil
}

var File_hal_proto protoreflect.FileDescriptor

var file_hal_proto_rawDesc = []byte{
	0x0a, 0x09, 0x68, 0x61, 0x6c, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x05, 0x78, 0x67, 0x72,
	0x70, 0x63, 0x1a, 0x1b, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f,
	0x62, 0x75, 0x66, 0x2f, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x22,
	0x9c, 0x01, 0x0a, 0x07, 0x4f, 0x70, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x12, 0x1d, 0x0a, 0x0a, 0x73,
	0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x05, 0x52,
	0x09, 0x73, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x49, 0x64, 0x12, 0x1e, 0x0a, 0x0a, 0x74, 0x69,
	0x6d, 0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x18, 0x02, 0x20, 0x01, 0x28, 0x05, 0x52, 0x0a,
	0x74, 0x69, 0x6d, 0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x12, 0x2c, 0x0a, 0x11, 0x6d, 0x6f,
	0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65, 0x64, 0x50, 0x65, 0x72, 0x63, 0x65, 0x6e, 0x74, 0x18,
	0x03, 0x20, 0x01, 0x28, 0x05, 0x52, 0x11, 0x6d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65,
	0x64, 0x50, 0x65, 0x72, 0x63, 0x65, 0x6e, 0x74, 0x12, 0x24, 0x0a, 0x06, 0x72, 0x65, 0x6c, 0x61,
	0x79, 0x73, 0x18, 0x04, 0x20, 0x03, 0x28, 0x0b, 0x32, 0x0c, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63,
	0x2e, 0x52, 0x65, 0x6c, 0x61, 0x79, 0x52, 0x06, 0x72, 0x65, 0x6c, 0x61, 0x79, 0x73, 0x22, 0x49,
	0x0a, 0x05, 0x52, 0x65, 0x6c, 0x61, 0x79, 0x12, 0x0e, 0x0a, 0x02, 0x49, 0x44, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x05, 0x52, 0x02, 0x49, 0x44, 0x12, 0x16, 0x0a, 0x06, 0x54, 0x69, 0x6d, 0x65, 0x4f,
	0x6e, 0x18, 0x02, 0x20, 0x01, 0x28, 0x05, 0x52, 0x06, 0x54, 0x69, 0x6d, 0x65, 0x4f, 0x6e, 0x12,
	0x18, 0x0a, 0x07, 0x54, 0x69, 0x6d, 0x65, 0x4f, 0x66, 0x66, 0x18, 0x03, 0x20, 0x01, 0x28, 0x05,
	0x52, 0x07, 0x54, 0x69, 0x6d, 0x65, 0x4f, 0x66, 0x66, 0x22, 0xe5, 0x02, 0x0a, 0x0e, 0x4f, 0x70,
	0x74, 0x69, 0x6f, 0x6e, 0x73, 0x43, 0x6f, 0x6d, 0x6d, 0x61, 0x6e, 0x64, 0x12, 0x16, 0x0a, 0x06,
	0x76, 0x6f, 0x6c, 0x75, 0x6d, 0x65, 0x18, 0x01, 0x20, 0x01, 0x28, 0x05, 0x52, 0x06, 0x76, 0x6f,
	0x6c, 0x75, 0x6d, 0x65, 0x12, 0x1d, 0x0a, 0x0a, 0x73, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x5f,
	0x69, 0x64, 0x18, 0x02, 0x20, 0x01, 0x28, 0x05, 0x52, 0x09, 0x73, 0x74, 0x61, 0x74, 0x69, 0x6f,
	0x6e, 0x49, 0x64, 0x12, 0x28, 0x0a, 0x0f, 0x53, 0x74, 0x61, 0x72, 0x74, 0x54, 0x69, 0x6d, 0x65,
	0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x18, 0x03, 0x20, 0x01, 0x28, 0x05, 0x52, 0x0f, 0x53, 0x74,
	0x61, 0x72, 0x74, 0x54, 0x69, 0x6d, 0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x12, 0x36, 0x0a,
	0x16, 0x53, 0x74, 0x61, 0x72, 0x74, 0x4d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65, 0x64,
	0x50, 0x65, 0x72, 0x63, 0x65, 0x6e, 0x74, 0x18, 0x04, 0x20, 0x01, 0x28, 0x05, 0x52, 0x16, 0x53,
	0x74, 0x61, 0x72, 0x74, 0x4d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65, 0x64, 0x50, 0x65,
	0x72, 0x63, 0x65, 0x6e, 0x74, 0x12, 0x2e, 0x0a, 0x0b, 0x53, 0x74, 0x61, 0x72, 0x74, 0x52, 0x65,
	0x6c, 0x61, 0x79, 0x73, 0x18, 0x05, 0x20, 0x03, 0x28, 0x0b, 0x32, 0x0c, 0x2e, 0x78, 0x67, 0x72,
	0x70, 0x63, 0x2e, 0x52, 0x65, 0x6c, 0x61, 0x79, 0x52, 0x0b, 0x53, 0x74, 0x61, 0x72, 0x74, 0x52,
	0x65, 0x6c, 0x61, 0x79, 0x73, 0x12, 0x26, 0x0a, 0x0e, 0x53, 0x74, 0x6f, 0x70, 0x54, 0x69, 0x6d,
	0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x18, 0x06, 0x20, 0x01, 0x28, 0x05, 0x52, 0x0e, 0x53,
	0x74, 0x6f, 0x70, 0x54, 0x69, 0x6d, 0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x12, 0x34, 0x0a,
	0x15, 0x53, 0x74, 0x6f, 0x70, 0x4d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65, 0x64, 0x50,
	0x65, 0x72, 0x63, 0x65, 0x6e, 0x74, 0x18, 0x07, 0x20, 0x01, 0x28, 0x05, 0x52, 0x15, 0x53, 0x74,
	0x6f, 0x70, 0x4d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65, 0x64, 0x50, 0x65, 0x72, 0x63,
	0x65, 0x6e, 0x74, 0x12, 0x2c, 0x0a, 0x0a, 0x53, 0x74, 0x6f, 0x70, 0x52, 0x65, 0x6c, 0x61, 0x79,
	0x73, 0x18, 0x08, 0x20, 0x03, 0x28, 0x0b, 0x32, 0x0c, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63, 0x2e,
	0x52, 0x65, 0x6c, 0x61, 0x79, 0x52, 0x0a, 0x53, 0x74, 0x6f, 0x70, 0x52, 0x65, 0x6c, 0x61, 0x79,
	0x73, 0x22, 0x38, 0x0a, 0x06, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x12, 0x16, 0x0a, 0x06, 0x61,
	0x6e, 0x73, 0x77, 0x65, 0x72, 0x18, 0x01, 0x20, 0x01, 0x28, 0x03, 0x52, 0x06, 0x61, 0x6e, 0x73,
	0x77, 0x65, 0x72, 0x12, 0x16, 0x0a, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x18, 0x02, 0x20,
	0x01, 0x28, 0x09, 0x52, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x22, 0x27, 0x0a, 0x0d, 0x41,
	0x6e, 0x73, 0x77, 0x65, 0x72, 0x43, 0x6f, 0x6d, 0x6d, 0x61, 0x6e, 0x64, 0x12, 0x16, 0x0a, 0x06,
	0x61, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x18, 0x01, 0x20, 0x01, 0x28, 0x03, 0x52, 0x06, 0x61, 0x6e,
	0x73, 0x77, 0x65, 0x72, 0x22, 0x27, 0x0a, 0x0d, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x50, 0x72,
	0x6f, 0x67, 0x72, 0x61, 0x6d, 0x12, 0x16, 0x0a, 0x06, 0x61, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x18,
	0x01, 0x20, 0x01, 0x28, 0x03, 0x52, 0x06, 0x61, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x22, 0x25, 0x0a,
	0x0b, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x4c, 0x65, 0x76, 0x65, 0x6c, 0x12, 0x16, 0x0a, 0x06,
	0x61, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x18, 0x01, 0x20, 0x01, 0x28, 0x03, 0x52, 0x06, 0x61, 0x6e,
	0x73, 0x77, 0x65, 0x72, 0x22, 0xa9, 0x01, 0x0a, 0x14, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74,
	0x53, 0x74, 0x6f, 0x70, 0x44, 0x69, 0x73, 0x70, 0x65, 0x6e, 0x73, 0x65, 0x72, 0x12, 0x1d, 0x0a,
	0x0a, 0x73, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28,
	0x05, 0x52, 0x09, 0x73, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x49, 0x64, 0x12, 0x1e, 0x0a, 0x0a,
	0x74, 0x69, 0x6d, 0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x18, 0x02, 0x20, 0x01, 0x28, 0x05,
	0x52, 0x0a, 0x74, 0x69, 0x6d, 0x65, 0x6f, 0x75, 0x74, 0x53, 0x65, 0x63, 0x12, 0x2c, 0x0a, 0x11,
	0x6d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70, 0x65, 0x65, 0x64, 0x50, 0x65, 0x72, 0x63, 0x65, 0x6e,
	0x74, 0x18, 0x03, 0x20, 0x01, 0x28, 0x05, 0x52, 0x11, 0x6d, 0x6f, 0x74, 0x6f, 0x72, 0x53, 0x70,
	0x65, 0x65, 0x64, 0x50, 0x65, 0x72, 0x63, 0x65, 0x6e, 0x74, 0x12, 0x24, 0x0a, 0x06, 0x72, 0x65,
	0x6c, 0x61, 0x79, 0x73, 0x18, 0x04, 0x20, 0x03, 0x28, 0x0b, 0x32, 0x0c, 0x2e, 0x78, 0x67, 0x72,
	0x70, 0x63, 0x2e, 0x52, 0x65, 0x6c, 0x61, 0x79, 0x52, 0x06, 0x72, 0x65, 0x6c, 0x61, 0x79, 0x73,
	0x32, 0xb6, 0x02, 0x0a, 0x13, 0x48, 0x61, 0x72, 0x64, 0x77, 0x61, 0x72, 0x65, 0x41, 0x63, 0x63,
	0x65, 0x73, 0x73, 0x4c, 0x61, 0x79, 0x65, 0x72, 0x12, 0x32, 0x0a, 0x0a, 0x52, 0x75, 0x6e, 0x50,
	0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0x12, 0x0e, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63, 0x2e, 0x4f,
	0x70, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x1a, 0x14, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63, 0x2e, 0x41,
	0x6e, 0x73, 0x77, 0x65, 0x72, 0x50, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0x12, 0x47, 0x0a, 0x18,
	0x4d, 0x65, 0x61, 0x73, 0x75, 0x72, 0x65, 0x56, 0x6f, 0x6c, 0x75, 0x6d, 0x65, 0x4d, 0x69, 0x6c,
	0x6c, 0x69, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x73, 0x12, 0x15, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63,
	0x2e, 0x4f, 0x70, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x43, 0x6f, 0x6d, 0x6d, 0x61, 0x6e, 0x64, 0x1a,
	0x14, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63, 0x2e, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x43, 0x6f,
	0x6d, 0x6d, 0x61, 0x6e, 0x64, 0x12, 0x39, 0x0a, 0x04, 0x53, 0x74, 0x6f, 0x70, 0x12, 0x1b, 0x2e,
	0x78, 0x67, 0x72, 0x70, 0x63, 0x2e, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x53, 0x74, 0x6f,
	0x70, 0x44, 0x69, 0x73, 0x70, 0x65, 0x6e, 0x73, 0x65, 0x72, 0x1a, 0x14, 0x2e, 0x78, 0x67, 0x72,
	0x70, 0x63, 0x2e, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x43, 0x6f, 0x6d, 0x6d, 0x61, 0x6e, 0x64,
	0x12, 0x2f, 0x0a, 0x06, 0x56, 0x6f, 0x6c, 0x75, 0x6d, 0x65, 0x12, 0x16, 0x2e, 0x67, 0x6f, 0x6f,
	0x67, 0x6c, 0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x45, 0x6d, 0x70,
	0x74, 0x79, 0x1a, 0x0d, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63, 0x2e, 0x41, 0x6e, 0x73, 0x77, 0x65,
	0x72, 0x12, 0x36, 0x0a, 0x08, 0x47, 0x65, 0x74, 0x4c, 0x65, 0x76, 0x65, 0x6c, 0x12, 0x16, 0x2e,
	0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e,
	0x45, 0x6d, 0x70, 0x74, 0x79, 0x1a, 0x12, 0x2e, 0x78, 0x67, 0x72, 0x70, 0x63, 0x2e, 0x41, 0x6e,
	0x73, 0x77, 0x65, 0x72, 0x4c, 0x65, 0x76, 0x65, 0x6c, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f,
	0x33,
}

var (
	file_hal_proto_rawDescOnce sync.Once
	file_hal_proto_rawDescData = file_hal_proto_rawDesc
)

func file_hal_proto_rawDescGZIP() []byte {
	file_hal_proto_rawDescOnce.Do(func() {
		file_hal_proto_rawDescData = protoimpl.X.CompressGZIP(file_hal_proto_rawDescData)
	})
	return file_hal_proto_rawDescData
}

var file_hal_proto_msgTypes = make([]protoimpl.MessageInfo, 8)
var file_hal_proto_goTypes = []interface{}{
	(*Options)(nil),              // 0: xgrpc.Options
	(*Relay)(nil),                // 1: xgrpc.Relay
	(*OptionsCommand)(nil),       // 2: xgrpc.OptionsCommand
	(*Answer)(nil),               // 3: xgrpc.Answer
	(*AnswerCommand)(nil),        // 4: xgrpc.AnswerCommand
	(*AnswerProgram)(nil),        // 5: xgrpc.AnswerProgram
	(*AnswerLevel)(nil),          // 6: xgrpc.AnswerLevel
	(*RequestStopDispenser)(nil), // 7: xgrpc.RequestStopDispenser
	(*emptypb.Empty)(nil),        // 8: google.protobuf.Empty
}
var file_hal_proto_depIdxs = []int32{
	1, // 0: xgrpc.Options.relays:type_name -> xgrpc.Relay
	1, // 1: xgrpc.OptionsCommand.StartRelays:type_name -> xgrpc.Relay
	1, // 2: xgrpc.OptionsCommand.StopRelays:type_name -> xgrpc.Relay
	1, // 3: xgrpc.RequestStopDispenser.relays:type_name -> xgrpc.Relay
	0, // 4: xgrpc.HardwareAccessLayer.RunProgram:input_type -> xgrpc.Options
	2, // 5: xgrpc.HardwareAccessLayer.MeasureVolumeMilliliters:input_type -> xgrpc.OptionsCommand
	7, // 6: xgrpc.HardwareAccessLayer.Stop:input_type -> xgrpc.RequestStopDispenser
	8, // 7: xgrpc.HardwareAccessLayer.Volume:input_type -> google.protobuf.Empty
	8, // 8: xgrpc.HardwareAccessLayer.GetLevel:input_type -> google.protobuf.Empty
	5, // 9: xgrpc.HardwareAccessLayer.RunProgram:output_type -> xgrpc.AnswerProgram
	4, // 10: xgrpc.HardwareAccessLayer.MeasureVolumeMilliliters:output_type -> xgrpc.AnswerCommand
	4, // 11: xgrpc.HardwareAccessLayer.Stop:output_type -> xgrpc.AnswerCommand
	3, // 12: xgrpc.HardwareAccessLayer.Volume:output_type -> xgrpc.Answer
	6, // 13: xgrpc.HardwareAccessLayer.GetLevel:output_type -> xgrpc.AnswerLevel
	9, // [9:14] is the sub-list for method output_type
	4, // [4:9] is the sub-list for method input_type
	4, // [4:4] is the sub-list for extension type_name
	4, // [4:4] is the sub-list for extension extendee
	0, // [0:4] is the sub-list for field type_name
}

func init() { file_hal_proto_init() }
func file_hal_proto_init() {
	if File_hal_proto != nil {
		return
	}
	if !protoimpl.UnsafeEnabled {
		file_hal_proto_msgTypes[0].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Options); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[1].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Relay); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[2].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*OptionsCommand); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[3].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Answer); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[4].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*AnswerCommand); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[5].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*AnswerProgram); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[6].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*AnswerLevel); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_hal_proto_msgTypes[7].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*RequestStopDispenser); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_hal_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   8,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_hal_proto_goTypes,
		DependencyIndexes: file_hal_proto_depIdxs,
		MessageInfos:      file_hal_proto_msgTypes,
	}.Build()
	File_hal_proto = out.File
	file_hal_proto_rawDesc = nil
	file_hal_proto_goTypes = nil
	file_hal_proto_depIdxs = nil
}
