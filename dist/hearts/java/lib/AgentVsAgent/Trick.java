/**
 * Autogenerated by Thrift Compiler (0.9.1)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package AgentVsAgent;

import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.SchemeFactory;
import org.apache.thrift.scheme.StandardScheme;

import org.apache.thrift.scheme.TupleScheme;
import org.apache.thrift.protocol.TTupleProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.EncodingUtils;
import org.apache.thrift.TException;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.server.AbstractNonblockingServer.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.Set;
import java.util.HashSet;
import java.util.EnumSet;
import java.util.Collections;
import java.util.BitSet;
import java.nio.ByteBuffer;
import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Trick implements org.apache.thrift.TBase<Trick, Trick._Fields>, java.io.Serializable, Cloneable, Comparable<Trick> {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("Trick");

  private static final org.apache.thrift.protocol.TField LEADER_FIELD_DESC = new org.apache.thrift.protocol.TField("leader", org.apache.thrift.protocol.TType.STRING, (short)1);
  private static final org.apache.thrift.protocol.TField PLAYED_FIELD_DESC = new org.apache.thrift.protocol.TField("played", org.apache.thrift.protocol.TType.LIST, (short)2);

  private static final Map<Class<? extends IScheme>, SchemeFactory> schemes = new HashMap<Class<? extends IScheme>, SchemeFactory>();
  static {
    schemes.put(StandardScheme.class, new TrickStandardSchemeFactory());
    schemes.put(TupleScheme.class, new TrickTupleSchemeFactory());
  }

  public String leader; // required
  public List<Card> played; // required

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    LEADER((short)1, "leader"),
    PLAYED((short)2, "played");

    private static final Map<String, _Fields> byName = new HashMap<String, _Fields>();

    static {
      for (_Fields field : EnumSet.allOf(_Fields.class)) {
        byName.put(field.getFieldName(), field);
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, or null if its not found.
     */
    public static _Fields findByThriftId(int fieldId) {
      switch(fieldId) {
        case 1: // LEADER
          return LEADER;
        case 2: // PLAYED
          return PLAYED;
        default:
          return null;
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, throwing an exception
     * if it is not found.
     */
    public static _Fields findByThriftIdOrThrow(int fieldId) {
      _Fields fields = findByThriftId(fieldId);
      if (fields == null) throw new IllegalArgumentException("Field " + fieldId + " doesn't exist!");
      return fields;
    }

    /**
     * Find the _Fields constant that matches name, or null if its not found.
     */
    public static _Fields findByName(String name) {
      return byName.get(name);
    }

    private final short _thriftId;
    private final String _fieldName;

    _Fields(short thriftId, String fieldName) {
      _thriftId = thriftId;
      _fieldName = fieldName;
    }

    public short getThriftFieldId() {
      return _thriftId;
    }

    public String getFieldName() {
      return _fieldName;
    }
  }

  // isset id assignments
  public static final Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> metaDataMap;
  static {
    Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);
    tmpMap.put(_Fields.LEADER, new org.apache.thrift.meta_data.FieldMetaData("leader", org.apache.thrift.TFieldRequirementType.REQUIRED, 
        new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRING        , "Position")));
    tmpMap.put(_Fields.PLAYED, new org.apache.thrift.meta_data.FieldMetaData("played", org.apache.thrift.TFieldRequirementType.REQUIRED, 
        new org.apache.thrift.meta_data.ListMetaData(org.apache.thrift.protocol.TType.LIST, 
            new org.apache.thrift.meta_data.StructMetaData(org.apache.thrift.protocol.TType.STRUCT, Card.class))));
    metaDataMap = Collections.unmodifiableMap(tmpMap);
    org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap(Trick.class, metaDataMap);
  }

  public Trick() {
  }

  public Trick(
    String leader,
    List<Card> played)
  {
    this();
    this.leader = leader;
    this.played = played;
  }

  /**
   * Performs a deep copy on <i>other</i>.
   */
  public Trick(Trick other) {
    if (other.isSetLeader()) {
      this.leader = other.leader;
    }
    if (other.isSetPlayed()) {
      List<Card> __this__played = new ArrayList<Card>(other.played.size());
      for (Card other_element : other.played) {
        __this__played.add(new Card(other_element));
      }
      this.played = __this__played;
    }
  }

  public Trick deepCopy() {
    return new Trick(this);
  }

  @Override
  public void clear() {
    this.leader = null;
    this.played = null;
  }

  public String getLeader() {
    return this.leader;
  }

  public Trick setLeader(String leader) {
    this.leader = leader;
    return this;
  }

  public void unsetLeader() {
    this.leader = null;
  }

  /** Returns true if field leader is set (has been assigned a value) and false otherwise */
  public boolean isSetLeader() {
    return this.leader != null;
  }

  public void setLeaderIsSet(boolean value) {
    if (!value) {
      this.leader = null;
    }
  }

  public int getPlayedSize() {
    return (this.played == null) ? 0 : this.played.size();
  }

  public java.util.Iterator<Card> getPlayedIterator() {
    return (this.played == null) ? null : this.played.iterator();
  }

  public void addToPlayed(Card elem) {
    if (this.played == null) {
      this.played = new ArrayList<Card>();
    }
    this.played.add(elem);
  }

  public List<Card> getPlayed() {
    return this.played;
  }

  public Trick setPlayed(List<Card> played) {
    this.played = played;
    return this;
  }

  public void unsetPlayed() {
    this.played = null;
  }

  /** Returns true if field played is set (has been assigned a value) and false otherwise */
  public boolean isSetPlayed() {
    return this.played != null;
  }

  public void setPlayedIsSet(boolean value) {
    if (!value) {
      this.played = null;
    }
  }

  public void setFieldValue(_Fields field, Object value) {
    switch (field) {
    case LEADER:
      if (value == null) {
        unsetLeader();
      } else {
        setLeader((String)value);
      }
      break;

    case PLAYED:
      if (value == null) {
        unsetPlayed();
      } else {
        setPlayed((List<Card>)value);
      }
      break;

    }
  }

  public Object getFieldValue(_Fields field) {
    switch (field) {
    case LEADER:
      return getLeader();

    case PLAYED:
      return getPlayed();

    }
    throw new IllegalStateException();
  }

  /** Returns true if field corresponding to fieldID is set (has been assigned a value) and false otherwise */
  public boolean isSet(_Fields field) {
    if (field == null) {
      throw new IllegalArgumentException();
    }

    switch (field) {
    case LEADER:
      return isSetLeader();
    case PLAYED:
      return isSetPlayed();
    }
    throw new IllegalStateException();
  }

  @Override
  public boolean equals(Object that) {
    if (that == null)
      return false;
    if (that instanceof Trick)
      return this.equals((Trick)that);
    return false;
  }

  public boolean equals(Trick that) {
    if (that == null)
      return false;

    boolean this_present_leader = true && this.isSetLeader();
    boolean that_present_leader = true && that.isSetLeader();
    if (this_present_leader || that_present_leader) {
      if (!(this_present_leader && that_present_leader))
        return false;
      if (!this.leader.equals(that.leader))
        return false;
    }

    boolean this_present_played = true && this.isSetPlayed();
    boolean that_present_played = true && that.isSetPlayed();
    if (this_present_played || that_present_played) {
      if (!(this_present_played && that_present_played))
        return false;
      if (!this.played.equals(that.played))
        return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  @Override
  public int compareTo(Trick other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;

    lastComparison = Boolean.valueOf(isSetLeader()).compareTo(other.isSetLeader());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetLeader()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.leader, other.leader);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetPlayed()).compareTo(other.isSetPlayed());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetPlayed()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.played, other.played);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }

  public _Fields fieldForId(int fieldId) {
    return _Fields.findByThriftId(fieldId);
  }

  public void read(org.apache.thrift.protocol.TProtocol iprot) throws org.apache.thrift.TException {
    schemes.get(iprot.getScheme()).getScheme().read(iprot, this);
  }

  public void write(org.apache.thrift.protocol.TProtocol oprot) throws org.apache.thrift.TException {
    schemes.get(oprot.getScheme()).getScheme().write(oprot, this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("Trick(");
    boolean first = true;

    sb.append("leader:");
    if (this.leader == null) {
      sb.append("null");
    } else {
      sb.append(this.leader);
    }
    first = false;
    if (!first) sb.append(", ");
    sb.append("played:");
    if (this.played == null) {
      sb.append("null");
    } else {
      sb.append(this.played);
    }
    first = false;
    sb.append(")");
    return sb.toString();
  }

  public void validate() throws org.apache.thrift.TException {
    // check for required fields
    if (leader == null) {
      throw new org.apache.thrift.protocol.TProtocolException("Required field 'leader' was not present! Struct: " + toString());
    }
    if (played == null) {
      throw new org.apache.thrift.protocol.TProtocolException("Required field 'played' was not present! Struct: " + toString());
    }
    // check for sub-struct validity
  }

  private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
    try {
      write(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(out)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private void readObject(java.io.ObjectInputStream in) throws java.io.IOException, ClassNotFoundException {
    try {
      read(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(in)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private static class TrickStandardSchemeFactory implements SchemeFactory {
    public TrickStandardScheme getScheme() {
      return new TrickStandardScheme();
    }
  }

  private static class TrickStandardScheme extends StandardScheme<Trick> {

    public void read(org.apache.thrift.protocol.TProtocol iprot, Trick struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TField schemeField;
      iprot.readStructBegin();
      while (true)
      {
        schemeField = iprot.readFieldBegin();
        if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { 
          break;
        }
        switch (schemeField.id) {
          case 1: // LEADER
            if (schemeField.type == org.apache.thrift.protocol.TType.STRING) {
              struct.leader = iprot.readString();
              struct.setLeaderIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 2: // PLAYED
            if (schemeField.type == org.apache.thrift.protocol.TType.LIST) {
              {
                org.apache.thrift.protocol.TList _list0 = iprot.readListBegin();
                struct.played = new ArrayList<Card>(_list0.size);
                for (int _i1 = 0; _i1 < _list0.size; ++_i1)
                {
                  Card _elem2;
                  _elem2 = new Card();
                  _elem2.read(iprot);
                  struct.played.add(_elem2);
                }
                iprot.readListEnd();
              }
              struct.setPlayedIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          default:
            org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
        }
        iprot.readFieldEnd();
      }
      iprot.readStructEnd();

      // check for required fields of primitive type, which can't be checked in the validate method
      struct.validate();
    }

    public void write(org.apache.thrift.protocol.TProtocol oprot, Trick struct) throws org.apache.thrift.TException {
      struct.validate();

      oprot.writeStructBegin(STRUCT_DESC);
      if (struct.leader != null) {
        oprot.writeFieldBegin(LEADER_FIELD_DESC);
        oprot.writeString(struct.leader);
        oprot.writeFieldEnd();
      }
      if (struct.played != null) {
        oprot.writeFieldBegin(PLAYED_FIELD_DESC);
        {
          oprot.writeListBegin(new org.apache.thrift.protocol.TList(org.apache.thrift.protocol.TType.STRUCT, struct.played.size()));
          for (Card _iter3 : struct.played)
          {
            _iter3.write(oprot);
          }
          oprot.writeListEnd();
        }
        oprot.writeFieldEnd();
      }
      oprot.writeFieldStop();
      oprot.writeStructEnd();
    }

  }

  private static class TrickTupleSchemeFactory implements SchemeFactory {
    public TrickTupleScheme getScheme() {
      return new TrickTupleScheme();
    }
  }

  private static class TrickTupleScheme extends TupleScheme<Trick> {

    @Override
    public void write(org.apache.thrift.protocol.TProtocol prot, Trick struct) throws org.apache.thrift.TException {
      TTupleProtocol oprot = (TTupleProtocol) prot;
      oprot.writeString(struct.leader);
      {
        oprot.writeI32(struct.played.size());
        for (Card _iter4 : struct.played)
        {
          _iter4.write(oprot);
        }
      }
    }

    @Override
    public void read(org.apache.thrift.protocol.TProtocol prot, Trick struct) throws org.apache.thrift.TException {
      TTupleProtocol iprot = (TTupleProtocol) prot;
      struct.leader = iprot.readString();
      struct.setLeaderIsSet(true);
      {
        org.apache.thrift.protocol.TList _list5 = new org.apache.thrift.protocol.TList(org.apache.thrift.protocol.TType.STRUCT, iprot.readI32());
        struct.played = new ArrayList<Card>(_list5.size);
        for (int _i6 = 0; _i6 < _list5.size; ++_i6)
        {
          Card _elem7;
          _elem7 = new Card();
          _elem7.read(iprot);
          struct.played.add(_elem7);
        }
      }
      struct.setPlayedIsSet(true);
    }
  }

}

